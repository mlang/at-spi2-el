;;; atspi.el --- Assistive Technology Servvce Provider Interface

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

(defconst atspi-registry-service "org.freedesktop.atspi.Registry"
  "The ATSPI DBus Registry service name.")

(defconst atspi-registry-path "/org/freedesktop/atspi/registry"
  "The DBus path of the ATSPI registry.")

(defconst atspi-registry-interface "org.freedesktop.atspi.Registry"
  "The ATSPI DBus Registry interface name.")

(defconst atspi-accessible-interface "org.freedesktop.atspi.Accessible"
  "The ATSPI DBus Accessible interface name.")

(defconst atspi-action-interface "org.freedesktop.atspi.Action"
  "The ATSPI DBus Action interface name.")

(defconst atspi-text-interface "org.freedesktop.atspi.Text"
  "The ATSPI DBus Text interface name.")

(defun atspi-get-role-from-integer (value)
  "Convert VALUE (a integer) to a ATSPI role (a symbol)."
  (check-type value integer)
  (nth value atspi-role-keywords))

(defun atspi-registry-get-applications ()
  "Gets all the currently registered applications.
Returns a list of service names."
  (dbus-call-method
   :session atspi-registry-service atspi-registry-path
   atspi-registry-interface "getApplications"))

(defvar atspi-registry-update-applications-signal-handler nil
  "If non-nil the signal handler information returned from `dbus-register-signal'.")

(defun atspi-registry-update-applicatiopns-handler (what service)
  "Informs us WHAT has changed about SERVICE."
  (message "Device %s was %S" service what))

(defun atspi-registry-install-update-applications-handler ()
  "Install `atspi-registry-update-applicatiopns-handler'."
  (setq atspi-registry-update-applications-signal-handler
        (dbus-register-signal
         :session atspi-registry-service atspi-registry-path
	 atspi-registry-interface "updateApplications"
	 'atspi-update-applicatiopns-handler)))

;;; Tree of accessible objects

(defun atspi-get-tree (service)
  "Transfers information about all accessible objects of a SERVICE."
  (dbus-call-method
   :session service "/org/freedesktop/atspi/tree"
   "org.freedesktop.atspi.Tree" "getTree"))

(defun atspi-tree-entry-get-path (tree-entry)
  "Return the DBus path of the accessible object described by TREE-ENTRY."
  (nth 0 tree-entry))

(defun atspi-tree-entry-get-parent (tree-entry)
  "Return the DBus path of the parent of the object described by TREE-ENTRY."
  (nth 1 tree-entry))

(defun atspi-tree-entry-get-children (tree-entry)
  "Return a list of DBus paths of the children of this TREE-ENTRY."
  (nth 2 tree-entry))

(defun atspi-tree-entry-get-interfaces (tree-entry)
  "Return list of interfaces supported by the object described by TREE-ENTRY."
  (nth 3 tree-entry))

(defun atspi-tree-entry-get-name (tree-entry)
  "Return the name (if any) of the accessible object described by TREE-ENTRY."
  (nth 4 tree-entry))

(defun atspi-tree-entry-get-role (tree-entry)
  "Return the role (a keyword) of the object described by TREE-ENTRY."
  (atspi-get-role-from-integer (nth 5 tree-entry)))

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
			   (atspi-tree-entry-get-interfaces tree-entry))
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
  (dbus-call-method :session service path atspi-text-interface "getText"
		    :int32 start :int32 end))

(defun atspi-text-get-character-count (service path)
  (dbus-get-property
   :session service path "org.freedesktop.atspi.Text" "characterCount"))

(defun atspi-text-get-caret-offset (application accessible)
  (dbus-call-method
   :session application accessible dbus-interface-properties "Get" "org.freedesktop.Text" "caretOffset"))



(defun atspi-call-accessible-method (service path method &rest args)
  (apply #'dbus-call-method
	 :session service path atspi-accessible-interface method args))

(defun atspi-accessible-get-relation-set (service path)
  "Get a set defining the relationship of accessible object PATH of SERVICE to
other accessible objects."
  (atspi-call-accessible-method service path "getRelationSet"))

(defun atspi-accessible-get-state (service path)
  "Get the current state of accessible object at PATH of SERVICE."
  (multiple-value-bind (lower upper)
      (atspi-call-accessible-method service path "getState")
    (let ((bit 31) stateset)
      (while (>= bit 0)
	(if (>= lower (expt 2.0 bit))
	    (setq stateset (cons (nth bit atspi-state-keywords) stateset)
		  lower (- lower (expt 2.0 bit))))
	(setq bit (1- bit)))
    stateset)))

(defun atspi-accessible-get-role (service path)
  "Get the Role indicating the type of ui role played by PATH of SERVICE."
  (atspi-get-role-from-integer
   (atspi-call-accessible-method service path "getRole")))

(defun atspi-accessible-get-role-name (service path)
  (atspi-call-accessible-method service path "getRoleName"))
  
(defun atspi-call-action-method (service path method &rest args)
  (apply #'dbus-call-method
	 :session service path atspi-action-interface method args))

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
  (let* ((actions (atspi-call-action-method service path "getActions"))
	 (index (position do-action actions :key 'car :test #'string=)))
    (atspi-call-action-method service path "doAction" :int32 index)))

(defun atspi-gedit-get-editable-text (service)
  (let ((objects (remove-if-not #'atspi-tree-entry-text-role-p
				(atspi-get-tree service))))
    (when (> (length objects) 0)
      (let ((object (car objects)))
	(atspi-text-get-character-count service (atspi-tree-entry-get-path object))))))

;;; Application Cache

(defvar atspi-applications nil
  "An alist.")

(defun atspi-applications (&optional reload)
  (if (and applications (not reload))
      atspi-applications
    (setq atspi-applications
	  (mapcar (lambda (service)
		    (cons service (atspi-get-tree service)))
		  (atspi-registry-get-applications)))))


(provide 'atspi)
;;; atspi.el ends here
