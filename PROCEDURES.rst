;; Combobulate's procedure system takes as input one node in a
;; tree-sitter tree and returns another based on one or more
;; *procedures* that apply for the particular task you are trying to
;; do.

;; A *procedure* is a set of rules, constraints and search parameters
;; that return another node --- the node you actually want. Instead of
;; writing imperative code to crawl the tree, you specify what you
;; want, and Combobulate will try to find it.


Here is one example:
`(:activation-nodes
  ((:nodes "pair" :find-parent "dictionary" :position at-or-in)
   (:node "dictionary" :position at-or-in))
  :match-query (dictionary (pair)+ @match)
  :remove-types ("comment"))
