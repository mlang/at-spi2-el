;;; atspi.el --- Assistive Technology Servvce Provider Interface for Emacs

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

;; 

;;; History:
;; 

;;; Code:

(require 'cl)
(require 'dbus)

;;;; Constants

(defconst atspi-prefix "org.freedesktop.atspi."
  "Common prefix for AT-SPI service and interface names.")

;;;; Assistive technologies registry

(defconst atspi-service-registry (concat atspi-prefix "Registry")
  "The AT-SPI Registry D-Bus service name.
For invocation of methods on this interface see `atspi-call-registry-method'.")

(defun atspi-available-p ()
  "Return non-nil if AT-SPI is available (the registry daemon is running)."
  (member atspi-service-registry (dbus-list-names :session)))

(defconst atspi-path-registry "/org/freedesktop/atspi/registry"
  "The D-Bus path of the AT-SPI registry.")

(defconst atspi-interface-registry atspi-service-registry
  "The AT-SPI Registry D-Bus interface name.")

(defun atspi-call-registry-method (method &rest args)
  "Call `atspi-interface-registry' METHOD with ARGS."
  (check-type method string)
  (apply #'dbus-call-method :session atspi-service-registry
	 atspi-path-registry atspi-interface-registry method args))

(defun atspi-registry-get-applications ()
  "Gets all the currently registered applications.
Returns a list of D-Bus service names."
  (dbus-ignore-errors (atspi-call-registry-method "getApplications")))

;;;;; Signal handler for updateApplications

(defcustom atspi-application-added-hook '(atspi-define-action-commands)
  "List of functions to call when a new application was added to the registry.
The D-Bus service name of the newly added application is passed as an argument.
For this hook to fire `atspi-registry-register-update-applications-handler'
needs to be called at some point, possibly from
`atspi-client-initialisation-hook' which is run by `atspi-client-initialize'."
  :type 'hook
  :options '(atspi-define-action-commands))

(defcustom atspi-application-removed-hook nil
  "List of functions to call when an application is removed from the registry.
For this hook to fire `atspi-registry-register-update-applications-handler'
needs to be called at some point, possibly from
`atspi-client-initialisation-hook' which is run by `atspi-client-initialize'."
  :type 'hook)

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

(atspi-define-signal registry update-applications
  atspi-service-registry atspi-path-registry
  "updateApplications" (what service)
  "Informs us WHAT has changed about SERVICE."
  (cond
   ((= what 0)
    (run-hook-with-args 'atspi-application-added-hook service))
   ((= what 1)
    (run-hook-with-args 'atspi-application-removed-hook service))))

(defcustom atspi-focus-changed-hook nil
  "List of functions to call when focus has changed.
Arguments passed are SERVICE and PATH of the accessible object that just
received focus.
For this hook to fire `atspi-event-focus-register-focus-handler' needs to be
called at some point, possibly from `atspi-client-initialisation-hook'
which is run by `atspi-client-initialize'."
  :type 'hook
  :options '(atspi-focus-changed-echo-function)
  :link '(function-link atspi-client-initialize)
  :link '(variable-link atspi-client-initialisation-hook)
  :link '(function-link atspi-event-register-focus-handler))

(defconst atspi-interface-event-focus (concat atspi-prefix "Event.Focus"))

(atspi-define-signal event-focus focus
  nil nil
  "focus" (&rest ignore)
  "Call `atspi-focus-changed-hook' when a focus signal is received."
  (run-hook-with-args
   'atspi-focus-changed-hook
   (dbus-event-service-name event) (dbus-event-path-name event)))

(defun atspi-tree-remove-accessible-handler (path)
  (let ((service (dbus-event-service-name event)))
    (message "%s of %s was removed" path service)))

(defun atspi-tree-install-remove-accessible-handler ()
  "Install `atspi-tree-remove-accessible-handler'."
  (setq atspi-tree-remove-accessible-handler
        (dbus-register-signal
         :session nil "/org/freedesktop/atspi/tree"
	 "org.freedesktop.atspi.Tree" "removeAccessible"
	 #'atspi-tree-remove-accessible-handler)))

;;;; Tree of accessible objects

(defconst atspi-interface-tree (concat atspi-prefix "Tree")
  "Tree of accessible objects D-Bus interface name.
For invocation see `atspi-call-tree-method'.")

(defconst atspi-path-tree "/org/freedesktop/atspi/tree"
  "The D-Bus path to talk to `atspi-interface-tree'.")

(defun atspi-call-tree-method (service method &rest args)
  "On SERVICE call `atspi-interface-tree' METHOD with ARGS."
  (check-type service string)
  (check-type method string)
  (apply #'dbus-call-method :session service atspi-path-tree
	 atspi-interface-tree method args))

(defun atspi-tree-get-tree (service)
  "Transfers information about all accessible objects of SERVICE."
  (atspi-call-tree-method service "getTree"))

(defsubst atspi-tree-entry-get-path (tree-entry)
  "Return the D-Bus path of the accessible object described by TREE-ENTRY."
  (nth 0 tree-entry))

(defvar atspi-applications nil
  "AT-SPI object cache.")

(defun atspi-applications (&optional reload)
  (if (and atspi-applications (not reload))
      atspi-applications
    (setq atspi-applications
	  (mapcar (lambda (service)
		    (cons service (atspi-tree-get-tree service)))
		  (atspi-registry-get-applications)))))

(defun atspi-tree-get-entry (service path)
  "Return the cache entry describing accessible of SERVICE located at PATH."
  (find path (cdr (or (assoc service (atspi-applications))
		      (assoc service (atspi-applications t))))
	:test #'string= :key #'atspi-tree-entry-get-path))

(defsubst atspi-tree-entry-get-parent (tree-entry)
  "Return the D-Bus path of the parent of the object described by TREE-ENTRY."
  (nth 1 tree-entry))

(defun atspi-accessible-parent-path (service path)
  (atspi-tree-entry-get-parent (atspi-tree-get-entry service path)))

(defsubst atspi-tree-entry-get-children (tree-entry)
  "Return a list of D-Bus paths of the children of this TREE-ENTRY."
  (nth 2 tree-entry))

(defun atspi-list-children-paths (service path)
  (atspi-tree-entry-get-children (atspi-tree-get-entry service path)))

(defun atspi-child-count (service path)
  (length (atspi-list-children-paths service path)))

(defsubst atspi-tree-entry-get-interface-names (tree-entry)
  "Return list of interfaces supported by the object described by TREE-ENTRY."
  (nth 3 tree-entry))

(defun atspi-list-accessible-interface-names (service path)
  (atspi-tree-entry-get-interface-names (atspi-tree-get-entry service path)))

(defsubst atspi-tree-entry-get-name (tree-entry)
  "Return the name (if any) of the accessible object described by TREE-ENTRY."
  (nth 4 tree-entry))

(defun atspi-accessible-name (service path)
  (interactive
   (let ((service (completing-read "Service: " (atspi-applications t) nil t)))
     (list service
	   (completing-read "Path: "
			    (cdr (assoc service (atspi-applications)))
			    nil t))))
  (let ((name (atspi-tree-entry-get-name (atspi-tree-get-entry service path))))
    (if (interactive-p)
	(if (> (length name) 0)
	    (message "Accessible name of %s%s is \"%s\"" service path name)
	  (message "No accessible name defined for %s%s" service path))
      name)))

(defconst atspi-role-keywords
  '(:invalid :accelerator-label :alter :animation :arrow :calendar :canvas
    :check-box :check-menu-item :color-chooser :column-header :combo-box
    :date-editor :desktop-icon :desktop-frame :dial :dialog :directory-pane
    :drawing-area :file-chooser :filler :focus-traversable :font-chooser
    :frame :glass-pane :html-container :icon :image :internal-frame :label
    :layered-pane :list :list-item :menu :menu-bar :menu-item :option-pane
    :page-tab :page-tab-list :panel :password-text :popup-menu :progress-bar
    :push-button :radio-button :radio-menu-item :root-pane :row-header
    :scroll-bar :scroll-pane :separator :slider :spin-button :split-pane
    :status-bar :table :table-cell :table-column-header :table-row-header
    :tearoff-menu-item :terminal :text :toggle-button :tool-bar :tool-tip
    :tree :tree-table :unknown :viewport :window :extended :header :footer
    :paragraph :ruler :application :autocomplete :editbar :embedded :entry
    :chart :caption :document-frame :heading :page :section :redundant-object
    :form :link :input-method-window)
  "List of object roles.")

(defun atspi-decode-role (value)
  "Convert VALUE (a integer) to an AT-SPI object role (a symbol)."
  (check-type value integer)
  (nth value atspi-role-keywords))

(defsubst atspi-tree-entry-get-role (tree-entry)
  "Return the role (a keyword) of the object described by TREE-ENTRY."
  (atspi-decode-role (nth 5 tree-entry)))

(defsubst atspi-tree-entry-get-description (tree-entry)
  "Return the description of the object described by TREE-ENTRY."
  (nth 6 tree-entry))

(defconst atspi-state-keywords
  '(:invalid :active :armed :busy :checked :collapsed :defunct :editable
    :enabled :expandable :expanded :focusable :focused :has-tooltip
    :horizontal :iconified :modal :multi-line :multiselectable :opaque
    :pressed :resizable :selectable :selected :sensitive :showing :single-line
    :stale :transient :vertical :visible :manages-descendants :indeterminate
    :required :truncated :animated :invalid-entry :supports-autocompletion
    :selectable-text :is-default :visited)
  "List of object states.")

(defun atspi-decode-state-bitfields (lower upper)
  "Decode LOWER and UPPER (32bit values) to a list of state keywords."
  ;; Bitwise operations on potential floating point values
  (let ((bit 31) stateset)
    (while (>= bit 0)
      (let ((amount (expt 2.0 bit)))
	(if (>= lower amount)
	    (setq stateset (cons (nth bit atspi-state-keywords) stateset)
		  lower (- lower amount)))
	(setq bit (1- bit))))
    stateset))

(defun atspi-tree-entry-get-states (tree-entry &rest allowed)
  "Return the states associated with the object described by TREE-ENTRY.
Optionally filter out those states not in ALLOWED."
  (let ((states (apply #'atspi-decode-state-bitfields (nth 7 tree-entry))))
    (if allowed
	(remove-if-not (lambda (state) (member state allowed)) states)
      states)))

(defun atspi-accessible-states (service path &rest allowed)
  (apply #'atspi-tree-entry-get-states
	 (atspi-tree-get-entry service path) allowed))

(mapcar (lambda (role)
	  (eval
	   `(defun ,(intern (concat "atspi-tree-entry-role-"
				    (substring (symbol-name role) 1)
				    "-p"))
	      (tree-entry)
	      (eq (atspi-tree-entry-get-role tree-entry) ,role))))
	atspi-role-keywords)

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

(defun atspi-get-application-path (service)
  (dbus-call-method
   :session service "/org/freedesktop/atspi/accessible"
   "org.freedesktop.atspi.Accessible" "getApplication"))

(defun atspi-get-root (application)
  (dbus-call-method
   :session application
   "/org/freedesktop/atspi/tree" "org.freedesktop.atspi.Tree" "getRoot"))

;;;; Accessible interfaces

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
		   (let ((func (intern (concat "atspi-" (symbol-name name) "-"
					       (symbol-name (nth 0 method)))))
			 (args (append (list 'service 'path) (nth 1 method))))
		     (append (list 'defun func args) (cddr method))))
		 methods))))

(defun atspi-read-service-and-path (&optional path-predicate)
  (let* ((service (completing-read "Service: "
				   (atspi-registry-get-applications) nil t))
	(path (completing-read "Object path: "
			       (atspi-tree-get-tree service) path-predicate
			       t)))
    (list service path)))

(atspi-define-accessible-interface accessible "Accessible"
  (get-relation-set ()
    "Get a set defining the relationship of accessible object PATH of SERVICE
to other accessible objects."
    (atspi-call-accessible-method service path "getRelationSet"))
  (get-states ()
    "Get the current state of accessible object SERVICE PATH via D-Bus."
    (apply #'atspi-decode-state-bitfields
	   (atspi-call-accessible-method service path "getState")))
  (get-role ()
    "Get the Role indicating the type of ui role played by PATH of SERVICE."
    (atspi-decode-role (atspi-call-accessible-method service path "getRole")))
  (get-role-name ()
    (atspi-call-accessible-method service path "getRoleName")))
  
(atspi-define-accessible-interface action "Action"
  (get-actions ()
    (atspi-call-action-method service path "getActions"))
  (do-action (action)
    "Invoke ACTION (a string or integer index) of Action object SERVICE PATH."
    (interactive
     (destructuring-bind (service path) (atspi-read-service-and-path
					 #'atspi-tree-entry-Action-p)
       (list service path
	     (completing-read "Action to invoke: "
			      (atspi-action-get-actions service path) nil t))))
    (when (stringp action)
      (let ((actions (atspi-action-get-actions service path))
	    (index (position action actions :key #'car :test #'string=)))
	(if index
	    (setq action index)
	  (error "Action \"%s\" is not defined" action))))
    (check-type action integer)
    (atspi-call-action-method service path "doAction" :int32 action)))

(atspi-define-accessible-interface text "Text"
  (get-text (&optional start end)
    "Obtain all or part of the textual content of a Text object."
    (unless start (setq start 0))
    (unless end (setq end -1))
    (atspi-call-text-method service path "getText" :int32 start :int32 end)))

(defun atspi-text-get-character-count (service path)
  (dbus-get-property
   :session service path atspi-interface-text "characterCount"))

(defun atspi-text-get-caret-offset (application accessible)
  (dbus-call-method
   :session application accessible dbus-interface-properties "Get" "org.freedesktop.Text" "caretOffset"))

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
  (let* ((actions (atspi-action-get-actions service path))
	 (index (position do-action actions :key 'car :test #'string=)))
    (atspi-call-action-method service path "doAction" :int32 index)))


(defun atspi-tree-find-entry (tree path)
  (find path tree :key #'car :test #'string=))

(defun atspi-define-action-commands (service)
  (interactive (list (completing-read "Service: "
				      (atspi-registry-get-applications))))
  (let ((tree (atspi-tree-get-tree service)))
    (dolist (action-object (remove-if-not #'atspi-tree-entry-Action-p tree))
      (let ((accessible-name (atspi-tree-entry-get-name action-object)))
	(when (> (length accessible-name) 0)
	  (let ((path (list accessible-name))
		(current-object action-object))
	    (while current-object
	      (setq current-object (atspi-tree-find-entry
				    tree
				    (atspi-tree-entry-get-parent
				     current-object)))
	      (when (and current-object
			 (> (length (atspi-tree-entry-get-name current-object)) 0))
		(setq path (cons (atspi-tree-entry-get-name current-object)
				 path))))
	    (let ((object-path (atspi-tree-entry-get-path action-object)))
	      (eval
	       `(defun ,(intern (mapconcat #'identity path " / ")) (action)
		  (interactive
		   (list (completing-read "Action to perform: "
					  (atspi-action-get-actions
					   ,service ,object-path))))
		  (let* ((actions (atspi-action-get-actions
				   ,service ,object-path))
			 (index (position action actions
					  :key 'car :test #'string=)))
		    (atspi-call-action-method ,service ,object-path "doAction"
					      :int32 index)))))))))))

(defun atspi-focus-changed-echo-function (service path)
  (let ((tree-entry (atspi-tree-get-entry service path)))
    (when tree-entry
      (let ((role (atspi-tree-entry-get-role tree-entry))
	    (name (atspi-tree-entry-get-name tree-entry))
	    (children (atspi-tree-entry-get-description tree-entry))
	    (description (atspi-tree-entry-get-description tree-entry)))
	(message (format "Focus now on a %s" role))))))
  
(defun espeak (string)
  (let ((proc (start-process "espeak" nil "espeak")))
    (process-send-string proc (concat string "\n"))
    (process-send-eof proc)))

;;;; Tree view

(require 'tree-widget)

(define-widget 'atspi-service 'tree-widget
  "AT-SPI widget to represent toplevel services."
  :tag "Service")

(defun atspi-tree-entry-to-widget (service tree-entry)
  (let ((children (atspi-tree-entry-get-children tree-entry))
	(name (atspi-tree-entry-get-name tree-entry))
	(path (atspi-tree-entry-get-path tree-entry)))
    (list 'tree-widget
	  :tag name
	  :service service
	  :path path
	  :has-children (> (length children) 0)
	  :dynargs (lambda (widget)
		     (let ((service (widget-get widget :service))
			   (path (widget-get widget :path)))
		       (mapcar (lambda (path)
				 (let ((entry (atspi-tree-get-entry service
								    path)))
				   (atspi-tree-entry-to-widget service entry)))
			       (atspi-tree-entry-get-children
				(atspi-tree-get-entry service path))))))))

(defun atspi-browser ()
  "Draw a tree of all accessible objects."
  (interactive)
  (switch-to-buffer "*AT-SPI Browser*")
  (kill-all-local-variables)
    (let ((inhibit-read-only t))
    (erase-buffer))
  ;(let ((all (tree-widget-sample-overlay-lists)))
  ;  (mapcar #'tree-widget-sample-delete-overlay (car all))
  ;  (mapcar #'tree-widget-sample-delete-overlay (cdr all)))

  (widget-insert (format "%s\n\n" (buffer-name)))
  (mapc (lambda (service)
	  (apply #'widget-create
	   'atspi-service :tag service
	   (list (atspi-tree-entry-to-widget service
					     (car (atspi-tree-get-tree service))))))
	(atspi-registry-get-applications)))

;;;; Client Initialisation

(defcustom atspi-client-initialisation-hook
  '(atspi-registry-register-update-applications-handler
    atspi-event-focus-register-focus-handler)
  "List of functions to call upon at-spi client initialisation."
  :type 'hook
  :options '(atspi-registry-register-update-applications-handler
	     atspi-event-register-focus-handler))

(defun atspi-client-initialize ()
  "Initialize signal handlers."
  (interactive)
  (if (not (atspi-available-p))
      (error "AT-SPI is not available.")
    (run-hooks 'atspi-client-initialisation-hook)
    t))

(provide 'atspi)
;;; atspi.el ends here
