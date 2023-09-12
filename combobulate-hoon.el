(require 'combobulate-rules)

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-interface)
(require 'combobulate-rules)

(declare-function combobulate--mark-node "combobulate-manipulation")
(declare-function combobulate-indent-region "combobulate-manipulation")
(defun combobulate-hoon--get-definition (node)
  (string-join
   (combobulate-query-node-text
    (pcase (combobulate-node-type node)
      ("luslusTall"
       '((luslusTall (Gap)* (name) @match))
       ))
    node
    t)
   ""))
(defun combobulate-hoon-pretty-print-node-name (node default-name)

  (pcase (combobulate-node-type node)
    (_ (thread-first node
                                 (combobulate-node-child 0)
                                 (message "child %s" it)
                                 (print)
                                 ))
    ))
(defun combobulate-hoon-calculate-indent (start end)
  (let ((indent-region-function nil))
    (combobulate-indent-region start end)))
(defun combobulate-hoon-setup (_)
  ;; no indent on envelopes.
  (setq combobulate-envelope-indent-region-function nil)
  (setq indent-region-function nil)
  (setq combobulate-manipulation-indent-after-edit nil)

  (setq combobulate-manipulation-edit-procedures
        '(;; Define location of definitions for various nodes
          ;; wrapFace: _skinWide "=" _hoonWide
 	  (:meta-node-arrow "⇒")
          ;; Determine name and body of the noted definitions
          (:name (-lhs$ _               ; Ignore auxiliary LHS $
                        reason:(_ /***/
                                  )+
                        decl:+
                        name))

	  (:body (or _- rhs:** decl (gap)* _+?)
	         (map symref (= mvar:@mvar:start-point (•1 conj:left-brace))))
          ;; edit expressions in cells
          (:activation-nodes
           ((:node "cell" :position at-or-in))
           :match-query (cell (_)+ @match))
          ;; edit  checks in factoryGates
          (:activation-nodes
           ((:node "factoryGate" :position at-or-in))
           :match-query (factoryGate (_)+ @match)
           :remove-types ("comment"))
          ;; edit items in wrapFace
          (:activation-nodes
           ((:node "wrapFace" :position at-or-in))
           :match-query (wrapFace (_)+ @match)
           :remove-types ("comment"))
          ;; edit items in composeExpression
          (:activation-nodes
           ((:node "composeExpressions" :position at-or-in))
           :match-query (composeExpressions (_)+ @match)
           :remove-types ("comment"))
          ;; edit items in wingPaths
          (:activation-nodes
           ((:node "wingPath" :position at-or-in))
           :match-query ((wingPath) (_)+ @match)
           :remove-types ("comment"))
          ;; edit arguments in gates
          (:activation-nodes
           ((:node "gateCall" :position at-or-in))
           :match-query (gateCall (_)+ @match)
           :remove-types ("comment"))
          ;; edit elements in paths
          (:activation-nodes
           ((:node "path" :position at-or-in))
           :match-query (path (_)+ @match)
           :remove-types ("comment"))
          ;; Edit definitions upon marking an edit node.
          )

        )

  (setq combobulate-navigation-sexp-nodes '("_hoonWide" "_hoonTall"
                                            "gateCall" "cell"))

  (setq combobulate-navigation-drag-parent-nodes '("increment"
                                                   "equality" "mold"
                                                   "cell" "gateCall"))
  (setq combobulate-navigation-sibling-procedures
      `((:activation-nodes
         ((:nod
           ,(combobulate-production-rules-get "barcabTall")
           :find-immmediate-parent ("barcabTall")
           :position at-or-in)
          (:node
           ,(combobulate-production-rules-get "luslusTall")
           :find-immmediate-parent ("luslusTall")
           :position at-or-in)
          )
         :remove-types ("Gap")
         :match-children t
         :match-siblings (:keep-parent t :keep-siblings t))

           (:activation-nodes
            ((:node
              ("comment")
              :position at-or-in)

	     )
	    :match-children t
            :match-siblings (:keep-parent t :keep-siblings t))


        ;; (:activation-nodes
        ;;   ((:node
        ;;     ("luslusTall" "lusbucTall" "buccolTall" "barcenTall" "barcabTall" )
        ;;     :position at-or-in)
        ;;   :remove-types ("tisgarTall" "tisfasTall" "tisgalTall" "comment" "Gap")
        ;;   :match-siblings (:keep-parent t :keep-siblings t))


        ;; ((:node
	;;   "normalize"
	;;   :position at-or-in
	;;   :find-immediate-parent ("valueWide" "normalize")))

        ;; (:node
        ;;  ,(append
        ;;    (combobulate-production-rules-get "cell"))
        ;;  :position at-or-in
        ;;  :find-immediate-parent ("cell"
        ;;                          ))
	;; (:node
	;;  ,(append
	;;    (combobulate-production-rules-get "term")
	;;    (combobulate-production-rules-get "tapeOrCord"))
	;;  :position at-or-in
	;;  :find-immediate-parent ("term" "tapeOrCord")
	;;  :match-children t
	;;  :remove-types ("comment"))
	;; ((:node
	;;   ,(append (combobulate-production-rules-get "source_file")
	;;            (combobulate-production-rules-get "bucgalTall")
	;;            '("source_file" "gateCall" ))
	;;   :position at-or-in
	;;   :find-immediate-parent ("source_file" "bucgalTall"))
        ;;  :remove-types "comment"
        ;;  :match-children t)
        ))

  (setq combobulate-manipulation-indent-after-edit nil)
  (setq combobulate-envelope-indent-region-function nil)
  (setq combobulate-manipulation-indent-method 'first)
  (setq combobulate-calculate-indent-function #'combobulate-baseline-indentation-default
        )
  (setq combobulate-navigation-defun-nodes '("luslusTall" "lusbucTall" "buccolTall" "barcenTall" "barcabTall" "tisfasTall" "tistarTall"))
  (setq combobulate-navigation-sexp-nodes '("cell" "gateCall" "path" "wingPath" "resolveWingWithChanges"))
    (setq combobulate-display-ignored-node-types '("Gap"))
  (setq combobulate-navigation-default-nodes
        (seq-uniq (append
                   combobulate-navigation-logical-nodes
                   combobulate-navigation-parent-child-nodes
		   combobulate-navigation-defun-nodes
		   combobulate-navigation-sexp-nodes)))
  (setq combobulate-navigation-parent-child-nodes
        (append
         '("luslusTall" "cell" "gateCall" "path" "wingPath" "resolveWingWithChanges" "barcabTall")))
  (setq combobulate-manipulation-splicing-procedures
        `((:activation-nodes
           ((:node
             ,(append "luslusTall")
             :find-base-rule-parent t
             :position at-or-in))
           :match-siblings (:keep-parent nil)))))
(provide 'combobulate-hoon)
