; Tree-sitter injection queries for language-javascript quasi-quoters.
;
; Enables JavaScript syntax highlighting inside [js|...|], [jsast|...|],
; and [jsx|...|] quasi-quote blocks in Haskell source files.
;
; Installation:
;   Copy this file to ~/.config/nvim/after/queries/haskell/injections.scm
;
; Requires:
;   - nvim-treesitter with Haskell and JavaScript parsers installed

; extends

(quasiquote
  (quoter) @_name
  (#any-of? @_name "js" "jsast" "jsx")
  (quasiquote_body) @injection.content
  (#set! injection.language "javascript"))
