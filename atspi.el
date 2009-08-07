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
    :tearoff-menu-item :terminal :text :toggle-button :tool-bar :tool-tip :tree
    :tree-table :unknown :viewport :window :extended :header :footer :paragraph
    :ruler :application :autocomplete :editbar :embedded :entry :chart
    :caption :document-frame :heading :page :section :redundant-object :form
    :link :input-method-window)
  "List of object roles.")

(defconst atspi-state-keywords
  '(:invalid :active :armed :busy :checked :collapsed :defunct :editable
    :enabled :expandable :expanded :focusable :focused :has-tooltip
    :horizontal :iconified :modal :multi-line :multiselectable :opaque
    :pressed :resizable :selectable :selected :sensitive :showing :single-line
    :stale :transient :vertical :visible :manages-descendants :indeterminate
    :required :truncated :animated :invalid-entry :supports-autocompletion
    :selectable-text :is-default :visited)
  "List of object states.")

(defconst atspi-prefix "org.freedesktop.atspi."
  "Common prefix for AT-SPI services and interfaces.")

(defconst atspi-service-registry (concat atspi-prefix "Registry")
  "The ATSPI DBus Registry service name.")

(defun atspi-available-p ()
  "Return non-nil when AT-SPI is available (the registry daemon is running)."
  (member atspi-service-registry (dbus-list-names :session)))

(defconst atspi-path-registry "/org/freedesktop/atspi/registry"
  "The DBus path of the ATSPI registry.")

(defconst atspi-interface-registry (concat atspi-prefix "Registry")
  "The ATSPI DBus Registry interface name.")

(defconst atspi-interface-accessible (concat atspi-prefix "Accessible")
  "The ATSPI DBus Accessible interface name.")

(defconst atspi-interface-action (concat atspi-prefix "Action")
  "The ATSPI DBus Action interface name.")

(defconst atspi-interface-text (concat atspi-prefix "Text")
  "The ATSPI DBus Text interface name.")

(defun atspi-decode-role (value)
  "Convert VALUE (a integer) to an AT-SPI object role (a symbol)."
  (check-type value integer)
  (nth value atspi-role-keywords))

(defun atspi-call-registry-method (method &rest args)
  "Call METHOD on the AT-SPI registry."
  (check-type method string)
  (apply #'dbus-call-method :session atspi-service-registry
	 atspi-path-registry atspi-interface-registry method args))

(defun atspi-registry-get-applications ()
  "Gets all the currently registered applications.
Returns a list of service names."
  (atspi-call-registry-method "getApplications"))

(defvar atspi-registry-update-applications-signal-handler nil
  "If non-nil the signal handler information from `dbus-register-signal'.")

(defun atspi-registry-update-applications-handler (what service)
  "Informs us WHAT has changed about SERVICE."
  (message "Device %s was %S" service what))

(defun atspi-registry-install-update-applications-handler ()
  "Register `atspi-registry-update-applications-handler' with D-Bus."
  (setq atspi-registry-update-applications-signal-handler
        (dbus-register-signal
         :session atspi-service-registry atspi-path-registry
	 atspi-interface-registry "updateApplications"
	 #'atspi-registry-update-applications-handler)))

(defvar atspi-event-focus-signal-handler nil
  "If non-nil the object returned from `dbus-register-signal'.")

(defcustom atspi-focus-changed-hook nil
  "A list of functions to execute when the focus has changed.
Arguments passed are SERVICE (the D-Bus service) and PATH (the object D-Bus
path)."
  :type 'hook)

(defun atspi-event-focus-handler (&rest ignore)
  "Call `atspi-focus-changed-hook' when a AT-SPI focus signal is received."
  (run-hook-with-args
   'atspi-focus-changed-hook
   (dbus-event-service-name event) (dbus-event-path-name event)))

(defun atspi-event-unregister-focus-handler ()
  "Uninstall `atspi-event-focus-handler'."
  (when atspi-event-focus-signal-handler
    (if (dbus-unregister-object atspi-event-focus-signal-handler)
	(setq atspi-event-focus-signal-handler nil)
      (display-warning
       'atspi "Failed to unregister `atspi-event-focus-signal-handler'"
       :error))))

(defun atspi-event-register-focus-handler ()
  "Install `atspi-event-focus-handler'."
  (atspi-event-unregister-focus-handler)
  (setq atspi-event-focus-signal-handler
	(dbus-register-signal
	 :session nil nil
	 "org.freedesktop.atspi.Event.Focus" "focus"
	 #'atspi-event-focus-handler)))

(defun atspi-tree-remove-accessible-handler (path)
  (let ((service (dbus-event-service-name event)))
    (message "%s of %s was removed" path service)))

(defun atspi-tree-install-remove-accessible-handler ()
  "Install `atspi-tree-remove-accessible-handler'."
  (setq atspi-tree-remove-accessible-handler
        (dbus-register-signal
         :session nil "/org/freedesktop/atspi/tree"
	 "org.freedesktop.atspi.Tree" "removeAccessible"
	 'atspi-tree-remove-accessible-handler)))

;;; Tree of accessible objects

(defun atspi-call-tree-method (service method &rest args)
  "Call Tree interface METHOD of SERVICE."
  (check-type service string)
  (apply #'dbus-call-method :session service
	 "/org/freedesktop/atspi/tree" "org.freedesktop.atspi.Tree" method args))

(defun atspi-get-tree (service)
  "Transfers information about all accessible objects of SERVICE."
  (atspi-call-tree-method service "getTree"))

(defun atspi-tree-get-entry (service path)
  "Return the cache entry describing accessible of SERVICE located at PATH."
  (find path (cdr (or (assoc service (atspi-applications))
		      (assoc service (atspi-applications t))))
	:test #'string= :key #'atspi-tree-entry-get-path))

(defun atspi-tree-entry-get-path (tree-entry)
  "Return the DBus path of the accessible object described by TREE-ENTRY."
  (nth 0 tree-entry))

(defun atspi-tree-entry-get-parent (tree-entry)
  "Return the DBus path of the parent of the object described by TREE-ENTRY."
  (nth 1 tree-entry))

(defun atspi-tree-entry-get-children (tree-entry)
  "Return a list of DBus paths of the children of this TREE-ENTRY."
  (nth 2 tree-entry))

(defun atspi-tree-entry-get-interface-names (tree-entry)
  "Return list of interfaces supported by the object described by TREE-ENTRY."
  (nth 3 tree-entry))

(defun atspi-tree-entry-get-name (tree-entry)
  "Return the name (if any) of the accessible object described by TREE-ENTRY."
  (nth 4 tree-entry))

(defun atspi-tree-entry-get-role (tree-entry)
  "Return the role (a keyword) of the object described by TREE-ENTRY."
  (atspi-decode-role (nth 5 tree-entry)))

(defun atspi-tree-entry-get-description (tree-entry)
  "Return the description of the object described by TREE-ENTRY."
  (nth 6 tree-entry))

(defun atspi-decode-state-bitfields (lower upper)
  (let ((bit 31) stateset)
    (while (>= bit 0)
      (let ((amount (expt 2.0 bit)))
	(if (>= lower amount)
	    (setq stateset (cons (nth bit atspi-state-keywords) stateset)
		  lower (- lower amount)))
	(setq bit (1- bit))))
    stateset))

(defun atspi-tree-entry-get-states (tree-entry)
  "Return the states associated with the object described by TREE-ENTRY."
  (apply #'atspi-decode-state-bitfields (nth 7 tree-entry)))

;; Predicates
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

(defun atspi-text-get-text (service path &optional start end)
  "Obtain all or part of the textual content of a Text object at PATH from SERVICE."
  (unless start (setq start 0))
  (unless end (setq end -1))
  (dbus-call-method :session service path atspi-interface-text "getText"
		    :int32 start :int32 end))

(defun atspi-text-get-character-count (service path)
  (dbus-get-property
   :session service path "org.freedesktop.atspi.Text" "characterCount"))

(defun atspi-text-get-caret-offset (application accessible)
  (dbus-call-method
   :session application accessible dbus-interface-properties "Get" "org.freedesktop.Text" "caretOffset"))


(defun atspi-call-accessible-method (service path method &rest args)
  "Call METHOD of Accessible object PATH of SERVICE."
  (apply #'dbus-call-method
	 :session service path atspi-interface-accessible method args))

(defun atspi-accessible-get-relation-set (service path)
  "Get a set defining the relationship of accessible object PATH of SERVICE to
other accessible objects."
  (atspi-call-accessible-method service path "getRelationSet"))

(defun atspi-accessible-get-states (service path)
  "Get the current state of accessible object at PATH of SERVICE."
  (apply #'atspi-decode-state-bitfields
	 (atspi-call-accessible-method service path "getState")))

(defun atspi-accessible-get-role (service path)
  "Get the Role indicating the type of ui role played by PATH of SERVICE."
  (atspi-decode-role (atspi-call-accessible-method service path "getRole")))

(defun atspi-accessible-get-role-name (service path)
  (atspi-call-accessible-method service path "getRoleName"))
  
(defun atspi-call-action-method (service path method &rest args)
  (apply #'dbus-call-method :session service
	 path atspi-interface-action method args))

(defun atspi-action-get-actions (service path)
  (atspi-call-action-method service path "getActions"))

(defun atspi-invoke-menu-item (service path &optional do-action)
  (interactive
   (let* ((application (completing-read "Application: "
					(atspi-registry-get-applications)))
	  (alist (mapcar (lambda (info)
			   (cons (atspi-tree-entry-get-name info)
				 (atspi-tree-entry-get-path info)))
			 (remove-if-not
			  #'atspi-tree-entry-role-menu-item-p
			  (atspi-get-tree application)))))
     (let ((action (completing-read "Menu item: " alist)))
       (list application (cdr (assoc action alist))))))
  (check-type service string)
  (check-type path string)
  (unless do-action (setq do-action "click"))
  (let* ((actions (atspi-action-get-actions service path))
	 (index (position do-action actions :key 'car :test #'string=)))
    (atspi-call-action-method service path "doAction" :int32 index)))


;;; Application Cache

(defvar atspi-applications nil
  "An alist.")

(defun atspi-applications (&optional reload)
  (if (and atspi-applications (not reload))
      atspi-applications
    (setq atspi-applications
	  (mapcar (lambda (service)
		    (cons service (atspi-get-tree service)))
		  (atspi-registry-get-applications)))))

(defun atspi-tree-find-entry (tree path)
  (find path tree :key #'car :test #'string=))

(defun atspi-define-action-commands (service)
  (let* ((tree (atspi-get-tree service))
	 (action-objects (remove-if-not #'atspi-tree-entry-Action-p tree)))
    (while action-objects
      (let ((action-object (car action-objects)))
	(when (> (length (atspi-tree-entry-get-name action-object)) 0)
	  (let ((path (list (atspi-tree-entry-get-name action-object)))
		(current-object action-object))
	    (while current-object
	      (setq current-object (atspi-tree-find-entry
				    tree
				    (atspi-tree-entry-get-parent current-object)))
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
					      :int32 index))))))))
      (setq action-objects (cdr action-objects)))))

(defun atspi-focus-changed-function (service path)
  (let ((tree-entry (atspi-tree-get-entry service path)))
    (when tree-entry
      (espeak (atspi-tree-entry-get-name tree-entry)))))
  
(defun espeak (string)
  (let ((proc (start-process "espeak" nil "espeak")))
    (process-send-string proc (concat string "\n"))
    (process-send-eof proc)))

(provide 'atspi)
;;; atspi.el ends here
