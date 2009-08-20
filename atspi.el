;;; atspi.el --- Assistive Technology Service Provider Interface for Emacs

;; Copyright (C) 2009  Mario Lang

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; For this file to be useful you need at least at-spi2-core and at-spi2-atk
;; from https://projects.codethink.co.uk/index.php/p/at-spi2/

;;; Code:

(require 'cl)
(require 'dbus)
(require 'warnings)

(defgroup atspi nil
  "Assistive technology service provider interface."
  :group 'external)

(defun atspi-debug (message &rest args)
  (let ((warning-minimum-log-level :debug)
	(warning-fill-prefix "  "))
    (display-warning 'atspi (apply #'format message args)
		     :debug "*AT-SPI Debug*")))

(defconst atspi-prefix "org.freedesktop.atspi."
  "Common prefix for AT-SPI D-Bus service and interface names.")

(defconst atspi-service-registry (concat atspi-prefix "Registry")
  "The AT-SPI Registry D-Bus service name.
For invocation of methods of this interface see `atspi-call-registry-method'.")

(defun atspi-available-p ()
  "Return non-nil if AT-SPI is available (the registry daemon is running)."
  (member atspi-service-registry (dbus-list-names :session)))

(defconst atspi-path-prefix "/org/freedesktop/atspi/"
  "Common prefix for AT-SPI D-Bus path names.")

(defconst atspi-path-registry (concat atspi-path-prefix "registry")
  "The D-Bus path of the AT-SPI registry.")

(defconst atspi-interface-registry atspi-service-registry
  "The AT-SPI D-Bus registry interface name.")

(defun atspi-call-registry-method (method &rest args)
  "Call `atspi-interface-registry' METHOD with ARGS."
  (check-type method string)
  (apply #'dbus-call-method :session atspi-service-registry
	 atspi-path-registry atspi-interface-registry method args))

(defun atspi-registry-get-applications ()
  "Retrieve a list of all the currently registered applications.
Return a list of D-Bus service names."
  (dbus-ignore-errors (atspi-call-registry-method "getApplications")))

(defcustom atspi-application-added-hook nil
  "List of functions to call when a new application was added to the registry.
The D-Bus service name of the newly added application is passed as an argument.
For this hook to work `atspi-client-mode' needs to be enabled."
  :type 'hook
  :link '(function-link atspi-registry-register-update-applications-handler))

(defcustom atspi-application-removed-hook nil
  "List of functions to call when an application was removed from the registry.
The first argument is the D-Bus service name of the application removed,
and the remaining arguments are the accessible objects just removed from
`atspi-cache'.
For this hook to work `atspi-client-mode' needs to be enabled."
  :type 'hook
  :link '(function-link atspi-registry-register-update-applications-handler))

(defun atspi-fill-docstring (column string)
  "Utility function to refill docstrings."
  (with-temp-buffer
    (let ((fill-column 76))
      (insert string) (fill-region (point-min) (point-max)) (buffer-string))))

(defmacro atspi-define-signal (type name service path signal args &rest body)
  "Define `atspi-TYPE-NAME-handler' for D-Bus SIGNAL of `atspi-interface-TYPE'.
Also define `atspi-TYPE-(un)register-NAME-handler' for handler activation
management."
  (let* ((prefix (concat "atspi-" (symbol-name type) "-"))
	 (name (symbol-name name))
	 (object (intern (concat prefix name "-signal-object")))
	 (interface (intern (concat "atspi-interface-" (symbol-name type))))
	 (handler (intern (concat prefix name "-handler")))
	 (register (intern (concat prefix "register-" name "-handler")))
	 (unregister (intern (concat prefix "unregister-" name "-handler"))))
    `(progn
       (defvar ,object nil
	 ,(atspi-fill-docstring
	   76 (format "If non-nil the object returned from `%s' when `%s' was
registered with D-Bus." 'dbus-register-signal handler)))
       (defun ,unregister ()
	 ,(format "Unregister `%s' from D-Bus." handler)
	 (when ,object
	   (if (dbus-unregister-object ,object)
	       (setq ,object nil)
	     (display-warning
	      'atspi ,(format "Failed to unregister `%s'" object) :error))))
       (defun ,handler ,args ,@body)
       (defun ,register ()
	 ,(atspi-fill-docstring
	   76 (format "Register `%s' for signal \"%s\" of `%s'."
		      handler signal interface))
	 (,unregister)
	 (setq ,object
	       (dbus-register-signal
		:session ,service ,path ,interface ,signal #',handler))))))

(defvar atspi-cache (make-hash-table :test 'equal)
  "AT-SPI object cache.
A hash-table where the key of each entry is a D-Bus service name and the value
is a hash-table where the key is a D-Bus path name and the value is an AT-SPI
object cache structure (a list) received via D-Bus.")

(defun atspi-cache-get-tree (service)
  "Same as `atspi-tree-get-tree' but only consider the local cache."
  (let ((table (gethash service atspi-cache)))
    (when service
      (let (tree)
	(maphash (lambda (path tree-entry) (push tree-entry tree)) table)
	tree))))

(defun atspi-cache-get (service path)
  "Return the cache entry describing accessible of SERVICE located at PATH."
  (let ((table (gethash service atspi-cache)))
    (when table
      (gethash path table))))

(defconst atspi-interface-tree (concat atspi-prefix "Tree")
  "Tree of accessible objects D-Bus interface name.
For invocation see `atspi-call-tree-method'.")

(defconst atspi-path-tree (concat atspi-path-prefix "tree")
  "The D-Bus path to `atspi-interface-tree'.")

(defun atspi-call-tree-method (service method &rest args)
  "On SERVICE call `atspi-interface-tree' METHOD with ARGS.
See also `atspi-tree-get-tree' for more specialised versions of this function."
  (check-type service string)
  (check-type method string)
  (apply #'dbus-call-method :session service atspi-path-tree
	 atspi-interface-tree method args))

(defun atspi-tree-get-tree (service)
  "Transfers information about all accessible objects of SERVICE.
Returns a list of accessible objects.  Each element is of the form
 (PATH PARENT CHILDREN INTERFACES NAME ROLE DESCRIPTION STATE)."
  (atspi-call-tree-method service "getTree"))

(defsubst atspi-tree-entry-get-path (tree-entry)
  "Return the D-Bus path of the accessible object described by TREE-ENTRY."
  (car tree-entry))

(defsubst atspi-tree-entry-get-parent (tree-entry)
  "Return the D-Bus path of the parent of accessible described by TREE-ENTRY."
  (nth 1 tree-entry))

(defun atspi-accessible-parent (service path)
  (if atspi-cache-mode
      (atspi-tree-entry-get-parent (atspi-cache-get service path))
    (atspi-get-property service path atspi-interface-accessible "parent")))

(defsubst atspi-tree-entry-get-children (tree-entry)
  "Return a list of D-Bus paths of the children of this TREE-ENTRY."
  (nth 2 tree-entry))

(defun atspi-accessible-children (service path)
  (if atspi-cache-mode
      (atspi-tree-entry-get-children (atspi-cache-get service path))
    (let ((count )
	  children)
      (dotimes (index (atspi-get-property service path
					  atspi-interface-accessible "childCount")
		      (nreverse children))
	(push (atspi-call-accessible-method service path
					    "getChildAtIndex" :int32 index)
	      children)))))

(defun atspi-accessible-child-count (service path)
  (if atspi-cache-mode
      (length (atspi-tree-entry-get-children (atspi-cache-get service path)))
    (atspi-get-property service path atspi-interface-accessible "childCount")))

(defsubst atspi-tree-entry-get-interface-names (tree-entry)
  "Return list of interfaces supported by the object described by TREE-ENTRY."
  (nth 3 tree-entry))

(defun atspi-accessible-interface-names (service path)
  (if atspi-cache-mode
      (atspi-tree-entry-get-interface-names (atspi-cache-get service path))
    (let ((tree (atspi-tree-get-tree service)) interfaces)
      (while (and tree (not interfaces))
	(if (string= (atspi-tree-entry-get-path (car tree)) path)
	    (setq interfaces (atspi-tree-entry-get-interface-names (car tree)))
	  (setq tree (cdr tree))))
      interfaces)))

(defsubst atspi-tree-entry-get-name (tree-entry)
  "Return the name (if any) of the accessible object described by TREE-ENTRY."
  (nth 4 tree-entry))

(defun atspi-accessible-name (service path)
  (interactive (atspi-read-service-and-path))
  (let ((name (if atspi-cache-mode
		  (atspi-tree-entry-get-name (atspi-cache-get service path))
		(atspi-get-property service path
				    atspi-interface-accessible "name"))))
    (if (interactive-p)
	(if (> (length name) 0)
	    (message "Accessible name of %s%s is \"%s\"" service path name)
	  (message "No accessible name defined for %s%s" service path))
      name)))

(defconst atspi-roles
  [invalid accelerator-label alert animation arrow calendar canvas check-box
   check-menu-item color-chooser column-header combo-box date-editor
   desktop-icon desktop-frame dial dialog directory-pane drawing-area
   file-chooser filler focus-traversable font-chooser frame glass-pane
   html-container icon image internal-frame label layered-pane list list-item
   menu menu-bar menu-item option-pane page-tab page-tab-list panel
   password-text popup-menu progress-bar push-button radio-button
   radio-menu-item root-pane row-header scroll-bar scroll-pane separator
   slider spin-button split-pane status-bar table table-cell
   table-column-header table-row-header tearoff-menu-item terminal text
   toggle-button tool-bar tool-tip tree tree-table unknown viewport window
   extended header footer paragraph ruler application autocomplete editbar
   embedded entry chart caption document-frame heading page section
   redundant-object form link input-method-window]
  "Object roles.")

(defun atspi-decode-role (value)
  "Convert VALUE (a integer) to an AT-SPI object role (a symbol)."
  (check-type value integer)
  (if (or (< value 0) (>= value (length atspi-roles)))
      (error "Role enumeration value out of bounds" value)
    (aref atspi-roles value)))

(defsubst atspi-tree-entry-get-role (tree-entry)
  "Return the role (a keyword) of the object described by TREE-ENTRY."
  (atspi-decode-role (nth 5 tree-entry)))

(defsubst atspi-tree-entry-get-description (tree-entry)
  "Return the description of the object described by TREE-ENTRY."
  (nth 6 tree-entry))

(defconst atspi-states
  [invalid active armed busy checked collapsed defunct editable enabled
   expandable expanded focusable focused has-tooltip horizontal iconified
   modal multi-line multiselectable opaque pressed resizable selectable
   selected sensitive showing single-line stale transient vertical visible
   manages-descendants indeterminate required truncated animated invalid-entry
   supports-autocompletion selectable-text is-default visited]
  "Object states.")

(defun atspi-decode-state-bitfields (lower upper)
  "Decode LOWER and UPPER (32bit values) to a list of state keywords."
  ;; Bitwise operations on potential floating point values
  (let ((bit 31) stateset)
    (while (>= bit 0)
      (let ((amount (expt 2.0 bit)))
	(if (>= lower amount)
	    (setq stateset (cons (aref atspi-states bit) stateset)
		  lower (- lower amount)))
	(setq bit (1- bit))))
    stateset))

(defun atspi-tree-entry-get-states (tree-entry &rest allowed)
  "Return the states associated with the object described by TREE-ENTRY.
Optionally filter out those states not in ALLOWED."
  (let ((states (apply #'atspi-decode-state-bitfields (nth 7 tree-entry))))
    (if allowed
	(remove-if-not (lambda (state) (memq state allowed)) states)
      states)))

(defun atspi-accessible-states (service path &rest allowed)
  (apply #'atspi-tree-entry-get-states (atspi-cache-get service path) allowed))

(mapcar (lambda (role)
	  (eval
	   `(defun ,(intern (concat "atspi-tree-entry-role-"
				    (symbol-name role) "-p"))
	      (tree-entry)
	      (eq (atspi-tree-entry-get-role tree-entry) ,role))))
	atspi-roles)

(mapcar (lambda (interface)
	  (eval
	   `(defun ,(intern (concat "atspi-tree-entry-"
				    (symbol-name interface)
				    "-p")) (tree-entry)
	     (when (member (concat "org.freedesktop.atspi."
				   ,(symbol-name interface))
			   (atspi-tree-entry-get-interface-names tree-entry))
	       t))))
	'(Action Application Component EditableText Text))

(defcustom atspi-accessible-added-hook '(atspi-log-cache-addition)
  "Hook run when an accessible cache entry is added.
Arguments are (SERVICE TREE-ENTRY)"
  :type 'hook
  :options '(atspi-log-cache-addition))

(defcustom atspi-accessible-updated-hook '(atspi-log-cache-update)
  "Hook run when an accessible cache entry was updated.
Arguments are (SERVICE OLD-TREE-ENTRY NEW-TREE-ENTRY)"
  :type 'hook
  :options '(atspi-log-cache-update))

(defun atspi-log-cache-addition (service tree-entry)
  (let ((path (atspi-tree-entry-get-path tree-entry)))
    (atspi-debug "New %s %s%s" (atspi-tree-entry-get-role tree-entry)
		 service path)))

(defun atspi-log-cache-update (service old-entry new-entry)
  (macrolet ((compare-cache-value (accessor comparator &rest body)
	       `(let ((old (,accessor old-entry))
		      (new (,accessor new-entry)))
		  (unless (,comparator old new) ,@body))))
    (let ((path (atspi-tree-entry-get-path new-entry)))
      (compare-cache-value atspi-tree-entry-get-parent string=
       (atspi-debug "%s%s parent changed from %s to %s"
		    service path old new))
      (compare-cache-value atspi-tree-entry-get-children equal
       (atspi-debug "%s%s children changed from %s to %s"
		    service path old new))
      (compare-cache-value atspi-tree-entry-get-interface-names equal
       (atspi-debug "%s%s interfaces changed from %s to %s"
		    service path old new))
      (compare-cache-value atspi-tree-entry-get-name string=
       (atspi-debug "%s%s accessible name changed from %s to %s"
		    service path old new))
      (compare-cache-value atspi-tree-entry-get-role eq
       (atspi-debug "%s%s role changed from %s to %s"
		    service path old new))
      (compare-cache-value atspi-tree-entry-get-description string=
       (atspi-debug "%s%s accessible description changed from %s to %s"
		    service path old new))
      (compare-cache-value atspi-tree-entry-get-states equal
       (atspi-debug "%s%s states changed from %s to %s"
		    service path old new)))))

(atspi-define-signal tree update-accessible
  nil atspi-path-tree "updateAccessible" (tree-entry)
  (let ((service (dbus-event-service-name last-input-event)))
    (atspi-cache-put service tree-entry)))

(defun atspi-cache-put (service tree-entry)
  (let ((path (atspi-tree-entry-get-path tree-entry))
	(table (gethash service atspi-cache))
	old-tree-entry)
    (if (not table)
	(puthash service
		 (let ((subtable (make-hash-table :test 'equal :size 200)))
		   (puthash path tree-entry subtable) subtable) atspi-cache)
      (setq old-tree-entry (gethash path table))
      (puthash path tree-entry table))
    (if (not old-tree-entry)
	(run-hook-with-args
	 'atspi-accessible-added-hook service tree-entry)
      (if (equal old-tree-entry tree-entry)
	  (display-warning
	   'atspi (format "Ignoring cache update with equal data on %s%s"
			  service path)
	   :debug)
	(run-hook-with-args
	 'atspi-accessible-updated-hook service old-tree-entry tree-entry)))))

(defun atspi-log-cache-removal (service tree-entry)
  (atspi-debug "Cache removal of %s: %S" service tree-entry))

(defcustom atspi-accessible-removed-hook '(atspi-log-cache-removal)
  "List of functions to call before a certain tree-entry is removed from
`atspi-cache'.
Arguments passed are (SERVICE TREE-ENTRY)."
  :type 'hook
  :options '(atspi-log-cache-removal))

(atspi-define-signal tree remove-accessible
  nil atspi-path-tree "removeAccessible" (path)
  (let ((service (dbus-event-service-name last-input-event)))
    (let ((table (gethash service atspi-cache)))
      (if (not table)
	  (display-warning
	   'atspi (format "Cache removal request for unknown service %s"
			  service)
	   :warning)
	(let ((tree-entry (gethash path table)))
	  (if (not tree-entry)
	      (display-warning
	       'atspi (format "Removal request for unknown object %s%s"
			      service path)
	       :warning)
	    (remhash path table)
	    (run-hook-with-args
	     'atspi-accessible-removed-hook service tree-entry)))))))

(defun atspi-get-application-path (service)
  (dbus-call-method
   :session service "/org/freedesktop/atspi/accessible"
   "org.freedesktop.atspi.Accessible" "getApplication"))

(defun atspi-get-root (application)
  (dbus-call-method
   :session application
   "/org/freedesktop/atspi/tree" "org.freedesktop.atspi.Tree" "getRoot"))

(defun atspi-cache-synchronise ()
  (let ((services (atspi-registry-get-applications)))
    (maphash (lambda (service hash-table)
	       (unless (member service services)
		 (atspi-cache-remove-service service))) atspi-cache)
    (dolist (service services)
      (let ((paths (mapcar #'atspi-tree-entry-get-path
			   (atspi-cache-get-tree service))))
	(dolist (tree-entry (atspi-tree-get-tree service))
	  (atspi-cache-put service tree-entry)
	  (setq paths (delete (atspi-tree-entry-get-path tree-entry) paths)))
	(mapc (lambda (path) (atspi-cache-remove-object service path))
	      paths)))))

(atspi-define-signal registry update-applications
  atspi-service-registry atspi-path-registry
  "updateApplications" (what service)
  "Informs us WHAT has changed about SERVICE."
  (check-type what (integer 0 1))
  (check-type service string)
  (cond
   ((= what 1)
    (when (gethash service atspi-cache)
      (display-warning
       'atspi (format "Add application request of already known service %s"
		      service)
       :warning))
    (run-hook-with-args 'atspi-application-added-hook service))
   ((= what 0)
    (unless (atspi-cache-remove-service service)
      (display-warning
       'atspi (format "Remove application request of unkown service %s"
		      service) :warning)))))

(defun atspi-cache-remove-service (service)
  (let ((table (gethash service atspi-cache)))
    (when table
      (let ((tree (atspi-cache-get-tree service)))
	(remhash service atspi-cache)
	(run-hook-with-args 'atspi-application-removed-hook tree)
	t))))

(defcustom atspi-focus-changed-hook nil
  "List of functions to call when focus has changed.
Arguments passed are SERVICE and PATH of the accessible object that just
received focus.
For this hook to work `atspi-client-mode' needs to be enabled."
  :type 'hook
  :options '(atspi-focus-changed-echo-function)
  :link '(function-link atspi-client-mode)
  :link '(function-link atspi-event-focus-register-focus-handler))

(defconst atspi-interface-event-focus (concat atspi-prefix "Event.Focus")
  "Interface for \"focus\" signals.")

(defvar atspi-locus-of-focus nil)

(atspi-define-signal event-focus focus
  nil nil
  "focus" (&rest ignore)
  "Call `atspi-focus-changed-hook' when a focus signal is received."
  (let ((service (dbus-event-service-name last-input-event))
	(path (dbus-event-path-name last-input-event)))
    (let ((locus (assoc service atspi-locus-of-focus)))
      (if (not locus)
	  (setq atspi-locus-of-focus
		(append (list (cons service path)) atspi-locus-of-focus))
	(setcdr locus path)))
    (run-hook-with-args 'atspi-focus-changed-hook service path)))

(defconst atspi-interface-event-window (concat atspi-prefix "Event.Window"))

(defcustom atspi-window-activated-hook nil
  "Hook run when a window is activated."
  :group 'atspi
  :type 'hook)

(defcustom atspi-window-deactivated-hook nil
  "Hook run when a window is deactivated."
  :group 'atspi
  :type 'hook)

(atspi-define-signal event-window activate
  nil nil
  "activate" (&rest ignore)
  "Call `atspi-window-activated-hook'."
  (let ((service (dbus-event-service-name last-input-event))
	(path (dbus-event-path-name last-input-event)))
    (atspi-debug "Window %s%s activated" service path)
    (run-hook-with-args 'atspi-window-activated-hook service path)))

(atspi-define-signal event-window deactivate
  nil nil
  "deactivate" (&rest ignore)
  "Call `atspi-window-deactivated-hook'."
  (let ((service (dbus-event-service-name last-input-event))
	(path (dbus-event-path-name last-input-event)))
    (atspi-debug "Window %s%s deactivated" service path)
    (run-hook-with-args 'atspi-window-deactivated-hook service path)))

;;;; Accessible interfaces

(defun atspi-define-accessible-defun-method (call-method name method)
  (let* ((func (intern (concat "atspi-" (symbol-name name) "-"
			      (symbol-name (nth 0 method)))))
	(args (append (list 'service 'path) (nth 1 method)))
	(docstring (and (stringp (nth 2 method)) (nth 2 method)))
	(interactive (or (and docstring (eq (car-safe (nth 3 method))
					    'interactive) (nth 3 method))
			 (and (eq (car-safe (nth 2 method))
				  'interactive) (nth 2 method))))
	(body (or (and docstring interactive (cddddr method))
		  (and (or docstring interactive) (cdddr method))
		  (cddr method))))
    (append
     (list 'defun func args)
     (and docstring (list docstring))
     (and interactive (list interactive))
     (list
      (append (list 'macrolet)
	      (list (list (list 'call-method '(method &rest args)
			  (list 'append 
				(list 'list (list 'quote call-method)
				      ''service ''path
				      'method) 'args))))
	      body)))))

(defmacro atspi-define-accessible-interface (name suffix &rest methods)
  "Define `atspi-interface-NAME' and `atspi-call-NAME-method'."
  (let ((call-method (intern (concat "atspi-call-"
				     (symbol-name name) "-method")))
	(interface (intern (concat "atspi-interface-" (symbol-name name)))))
    `(progn
       (defconst ,interface (concat atspi-prefix ,suffix)
	 ,(format "The AT-SPI %s D-Bus interface name.
To invoke this interface use `%s'." suffix call-method))
       (defun ,call-method (service path method &rest args)
	 ,(format "On SERVICE PATH call `%s' METHOD with optional ARGS."
		  interface)
	 (apply #'dbus-call-method :session service path
		,interface method args))
       ,@(mapcar (lambda (method)
		   (atspi-define-accessible-defun-method
		    call-method name method)) methods))))

(put 'atspi-define-accessible-interface 'lisp-indent-function 2)

(defun atspi-read-service-and-path (&optional path-predicate)
  "Prompt user for a D-Bus service and path of an AT-SPI Accessible object."
  (let* ((service (completing-read "Service: "
				   (atspi-registry-get-applications) nil t))
	(path (completing-read "Object path: "
			       (atspi-tree-get-tree service) path-predicate
			       t)))
    (list service path)))

(defconst atspi-relation-types
  [null             ; Not a meaningful relationship; clients should not
                    ; normally encounter this value.
   label-for        ; Object is a label for one or more other objects
   labelled-by      ; Object is labelled by one or more other objects.
   controller-for   ; Object is an interactive object which modifies the
                    ; state, onscreen location, or other attributes of one or
                    ; more target objects.
   controlled-by    ; Object state, position, etc. is modified/controlled by
		    ; user interaction with one or more other objects. For
		    ; instance a viewport or scroll pane may be :controlled-by
		    ; scrollbars.
   member-of        ; Object has a grouping relationship (e.g. 'same group as')
	  	    ; to one or more other objects.
   tooltip-for      ; Object is a tooltip associated with another object.
   node-child-of    ; Reserved for future use.
   extended         ; Used to indicate that a relationship exists, but its type
                    ; is not specified in the enumeration and must be obtained
                    ; via a call to getRelationTypeName.
   flows-to         ; Object renders content which flows logically to another
                    ; object.  For instance, text in a paragraph may flow to
                    ; another object which is not the        'next sibling' in
                    ; the accessibility hierarchy.
   flows-from       ; Reciprocal of flows-to
   subwindow-of     ; Object is visually and semantically considered a
                    ; subwindow of another object, even though it is not the
                    ; object's child.  Useful when dealing with embedded
                    ; applications and other cases where the widget hierarchy
                    ; does not map cleanly to the onscreen presentation.
   embeds           ; Similar to :subwindow-of, but specifically used for
                    ; cross-process embedding.
   embedded-by      ; Reciprocal of :embeds; Used to denote content rendered
                    ; by embedded renderers that live in a separate process
                    ; space from the embedding context.
   popup-for        ; Denotes that the object is a transient window or frame
                    ; associated with another onscreen object.  Similar to
                    ; :tooltip-for, but more general.  Useful for windows which
                    ; are technically toplevels but which, for one or more
                    ; reasons, do not explicitly cause their associated window
                    ; to lose 'window focus'.  Creation of a ROLE_WINDOW object
                    ; with the :popup-for relation usually requires some
                    ; presentation action on the part of assistive technology
                    ; clients, even though the previous toplevel ROLE_FRAME
                    ; object may still be the active window.
   parent-window-of ; This is the reciprocal relatipnship to :popup-for
   description-for  ; Indicates that an object provides descriptive
                    ; information about another object; more verbose than
                    ; label-for
   described-by     ; Indicates that another object provides descriptive
                    ; information about this object; more verbose than
                    ; labelled-by
   ]
  "Specifies a relationship between objects (possibly one-to-many or
many-to-one) outside of the normal parent/child hierarchical relationship.  It
allows better semantic identification of how objects are associated with one
another.
For instance the :labelled-by relationship may be used to identify labelling
information that should accompany the accessibleName property when presenting
an object's content or identity to the end user.  Similarly, :controller-for
can be used to further specify the context in which a valuator is useful,
and/or the other UI components which are directly effected by user interactions
with the valuator.  Common examples include association of scrollbars with the
viewport or panel which they control.")

(defun atspi-decode-relation-type (value)
  "Convert VALUE (a integer) to a AT-SPI relation type (a symbol)."
  (check-type value integer)
  (if (or (< value 0) (>= value (length atspi-relation-types)))
      (error "Relation type enumeration value out of bounds" value)
    (when (= value 0)
      (display-warning
       'atspi "Encountered null relation type value" :warning))
    (aref atspi-relation-types value)))

(atspi-define-accessible-interface accessible "Accessible"
  (get-relation-set ()
    "Get a set defining the relationship of accessible object PATH of SERVICE
to other accessible objects."
    (mapcar (lambda (relation)
	      (cons (atspi-decode-relation-type (car relation))
		    (cadr relation)))
	    (call-method "getRelationSet")))
  (get-states ()
    "Get the current state of accessible object SERVICE PATH.
Only transfers data via D-Bus if `atspi-cache-mode' is not enabled."
    (if atspi-cache-mode
	(atspi-tree-entry-get-states (atspi-cache-get service path))
      (apply #'atspi-decode-state-bitfields (call-method "getState"))))
  (get-role ()
    "Get the Role indicating the type of ui role played by PATH of SERVICE.
Only transfers data via D-Bus if `atspi-cache-mode' is not enabled."
    (if atspi-cache-mode
	(atspi-tree-entry-get-role (atspi-cache-get service path))
      (atspi-decode-role (call-method "getRole"))))
  (get-role-name ()
    (call-method "getRoleName"))
  (get-application ()
    "Get the containing Application object path for this object."
    (call-method "getApplication")))
  
(atspi-define-accessible-interface application "Application")

(defun atspi-toolkit-name (service &optional path)
  "A string indicating the type of user interface toolkit which is used by
the application.
Ordinarily clients should be toolkit-agnostic, dependencies on this property
should be avoided where possible."
  (check-type service string)
  (unless path (setq path (concat atspi-path-prefix "accessible")))
  (check-type path string)
  (unless (atspi-interface-implemented-p
	   service path atspi-interface-application)
    (setq path (atspi-accessible-get-application service path)))
  (atspi-get-property service path atspi-interface-application "toolkitName"))

(defun atspi-toolkit-version (service &optional path)
  "A string indicating the version number of the application's accessibility
bridge implementation."
  (check-type service string)
  (unless path (setq path "/org/freedesktop/atspi/accessible"))
  (check-type path string)
  (unless (atspi-interface-implemented-p
	   service path atspi-interface-application)
    (setq path (atspi-accessible-get-application service path)))
  (atspi-get-property service path atspi-interface-application "version"))

(atspi-define-accessible-interface action "Action"
  (get-actions ()
    (call-method "getActions"))
  (do-action (action)
    "Invoke ACTION (a string or integer index) of Action object SERVICE PATH."
    (interactive
     (destructuring-bind (service path)
	 (atspi-read-service-and-path #'atspi-tree-entry-Action-p)
       (list service path
	     (completing-read "Action to invoke: "
			      (atspi-action-get-actions service path) nil t))))
    (when (stringp action)
      (let* ((actions (atspi-action-get-actions service path))
	     (index (position action actions :key #'car :test #'string=)))
	(if index
	    (setq action index)
	  (error "Action \"%s\" is not defined" action))))
    (check-type action integer)
    (call-method "doAction" :int32 action)))

(defun atspi-interface-implemented-p (service path interface)
  (member interface (atspi-list-accessible-interface-names service path)))

(defun atspi-action-n-actions (service path)
  (unless (atspi-interface-implemented-p service path atspi-interface-action)
    (error "%s%s does not implement %s" service path atspi-interface-action))
  (atspi-get-property service path atspi-interface-action "nActions"))

(defconst atspi-coord-type-keywords '(:screen :window))

(defun atspi-decode-coord-type (value)
  (check-type value integer)
  (nth value atspi-coord-type-keywords))

(defun atspi-encode-coord-type (keyword)
  (position keyword atspi-coord-type-keywords))

(atspi-define-accessible-interface component "Component"
  (get-extents (&optional relative-to)
    (unless relative-to (setq relative-to :screen))
    (unless (integerp relative-to)
      (setq relative-to (atspi-encode-coord-type relative-to)))
    (call-method "getExtents" :uint32 relative-to))
  (grab-focus ()
    "Request that the object obtain keyboard focus.
Return t if keyboard focus was successfully transferred to the Component,
nil otherwise."
    (= (call-method "grabFocus") 0)))

(atspi-define-accessible-interface text "Text"
  (get-text (&optional start end)
    "Obtain all or part of the textual content of a Text object."
    (unless start (setq start 0))
    (unless end (setq end -1))
    (call-method "getText" :int32 start :int32 end))
  (set-caret-offset (position)
    (check-type position integer)
    (call-method "setCaretOffset" :int32 position)))

(defun atspi-get-property (service path interface property)
  (car
   (dbus-call-method
    :session service path dbus-interface-properties "Get" interface property)))

(defun atspi-text-get-character-count (service path)
  (atspi-get-property service path atspi-interface-text "characterCount"))

(defun atspi-text-get-caret-offset (service path)
  (atspi-get-property service path atspi-interface-text "caretOffset"))

(atspi-define-accessible-interface editable-text "EditableText"
  (set-text-contents (string)
    "Replace the text contents with a new STRING, discarding the old contents.
Return t if the text content was successfully changed, nil otherwise."
    (interactive
     (destructuring-bind (service path)
	 (atspi-read-service-and-path #'atspi-tree-entry-EditableText-p)
       (list service path
	     (read-string "New text contents: "))))
    (check-type string string)
    (let ((result (atspi-call-editable-text-method
		   service path "setTextContents"
		   (encode-coding-string string 'utf-8))))
      (if (interactive-p)
	  (if result
	      (message "Text content changed successfully")
	    (message "Failed to change text content"))
	result)))
  (insert-text (position string)
    "At POSITION (a integer) insert STRING into an EditableText object."
    (atspi-call-editable-text-method
     service path "insertText"
     :int32 position
     :string (encode-coding-string string 'utf-8)
     :int32 (length string))))

(defun atspi-invoke-menu-item (service path &optional do-action)
  (interactive
   (let* ((application (completing-read "Service: "
					(atspi-registry-get-applications)))
	  (alist (mapcar (lambda (info)
			   (cons (atspi-tree-entry-get-name info)
				 (atspi-tree-entry-get-path info)))
			 (remove-if-not
			  #'atspi-tree-entry-role-menu-item-p
			  (atspi-tree-get-tree application)))))
     (let ((action (completing-read "Menu item: " alist)))
       (list application (cdr (assoc action alist))))))
  (check-type service string)
  (check-type path string)
  (unless do-action (setq do-action "click"))
  (atspi-action-do-action service path do-action))

(defun atspi-tree-find-entry (tree path)
  (check-type path string)
  (find path tree :key #'atspi-tree-entry-get-path :test #'string=))

(defun atspi-tree-entry-get-all-parents (service tree-entry)
  (let ((path ()))
    (while tree-entry
      (setq path (cons tree-entry path)
	    tree-entry (atspi-cache-get
			service (atspi-tree-entry-get-parent tree-entry))))
    path))

(defun atspi-tree-entry-named-path (service tree-entry)
  (remove-if-not (lambda (entry)
		   (> (length (atspi-tree-entry-get-name entry)) 0))
		 (atspi-tree-entry-get-all-parents service tree-entry)))

(defun atspi-define-action-commands (service)
  (interactive (list (completing-read "Service: "
				      (atspi-registry-get-applications))))
  (let ((tree (atspi-tree-get-tree service)))
    (dolist (action-object (remove-if-not #'atspi-tree-entry-Action-p tree))
      (let ((accessible-name (atspi-tree-entry-get-name action-object)))
	(when (> (length accessible-name) 0)
	  (let ((object-path (atspi-tree-entry-get-path action-object)))
	    (eval
	     `(defun ,(intern (mapconcat #'atspi-tree-entry-get-name
					 (atspi-tree-entry-named-path
					  action-object tree)
					 " / ")) (action)
		 (interactive
		  (list (completing-read "Action to perform: "
					 (atspi-action-get-actions
					  ,service ,object-path))))
		 (atspi-action-do-action ,service ,object-path action)))))))))

(defun atspi-focus-changed-echo-function (service path)
  (let ((tree-entry (atspi-cache-get service path)))
    (when tree-entry
      (let ((role (atspi-tree-entry-get-role tree-entry))
	    (named-path (atspi-tree-entry-named-path service tree-entry))
	    (children (atspi-tree-entry-get-children tree-entry))
	    (description (atspi-tree-entry-get-description tree-entry)))
	(message (format "Focus now on %s (role %s)"
			 (mapconcat #'atspi-tree-entry-get-name named-path "/")
			 role))))))
  
(defun espeak (string)
  (let ((proc (start-process "espeak" nil "espeak")))
    (process-send-string proc (concat string "\n"))
    (process-send-eof proc)))

;;;; Tree view

(require 'tree-widget)

(defun atspi-service-widget-expander (widget)
  (let ((service (widget-get widget :service)))
    (mapcar (lambda (tree-entry)
	      (atspi-tree-entry-to-widget service tree-entry))
	    (atspi-tree-toplevel-entries (atspi-tree-get-tree service)))))

(define-widget 'atspi-service 'tree-widget
  "AT-SPI widget to represent toplevel services."
  :expander #'atspi-service-widget-expander)

(defsubst atspi-widget-apply (widget function &rest args)
  (apply function (widget-get widget :service) (widget-get widget :path) args))

(defun atspi-widget-expander (widget)
  (let ((service (widget-get widget :service)))
    (mapcar (lambda (path)
	      (let ((entry (atspi-cache-get service path)))
		(atspi-tree-entry-to-widget service entry)))
	    (atspi-tree-entry-get-children
	     (atspi-widget-apply widget #'atspi-cache-get)))))
(defun atspi-tree-entry-to-widget (service tree-entry)
  (let ((children (atspi-tree-entry-get-children tree-entry))
	(name (atspi-tree-entry-get-name tree-entry))
	(role (atspi-tree-entry-get-role tree-entry))
	(path (atspi-tree-entry-get-path tree-entry)))
    (when (not (> (length name) 0)) (setq name nil))
    (setq role (symbol-name role))
    (list
     'tree-widget
     :tag (or name role)
     :service service
     :path path
     :expander (when (> (length children) 0) #'atspi-widget-expander))))

(defconst atspi-path-accessible-root "/org/freedesktop/atspi/accessible/root")

(defun atspi-tree-toplevel-entries (tree)
  (remove-if-not (lambda (entry) (string= (atspi-tree-entry-get-parent entry)
					  atspi-path-accessible-root))
		 tree))

(define-derived-mode atspi-browser-mode special-mode "AT-SPI"
  "Major mode for interacting with an external network utility."
  (widget-minor-mode 1))

(defun atspi-browser ()
  "Draw a tree of all accessible objects."
  (interactive)
  (switch-to-buffer "*AT-SPI Browser*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (atspi-browser-mode)

  (widget-insert (format "%s\n\n" (buffer-name)))
  (mapc (lambda (service)
	  (widget-create 'atspi-service :tag service :service service))
	(atspi-registry-get-applications)))

(define-minor-mode atspi-cache-mode
  "Assistive technology application cache mode.
If this mode is on `atspi-cache' is kept up-to-date automatically."
  :global t :group 'atspi :require 'atspi :lighter " ATC"
  (if atspi-cache-mode
      (if (not (atspi-available-p))
	  (error "The AT-SPI registry is not available.")
	(atspi-cache-synchronise)
	(atspi-registry-register-update-applications-handler)
	(atspi-tree-register-update-accessible-handler)
	(atspi-tree-register-remove-accessible-handler)
	(atspi-event-focus-register-focus-handler)
	(atspi-event-window-register-activate-handler)
	(atspi-event-window-register-deactivate-handler))
    (atspi-registry-unregister-update-applications-handler)
    (atspi-tree-unregister-update-accessible-handler)
    (atspi-tree-unregister-remove-accessible-handler)
    (atspi-event-focus-unregister-focus-handler)
    (atspi-event-window-unregister-activate-handler)
    (atspi-event-window-unregister-deactivate-handler)))

(provide 'atspi)
;;; atspi.el ends here
