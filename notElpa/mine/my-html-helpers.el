;;; my-html-helpers.el --- helper funcs for html -*- lexical-binding: t -*-


(defvar my-html-yasnippets
  '("a" "abbr" "address" "aemail" "area" "article" "aside" "audio" "b" "base" "blockquote" "body" "br" "button"
    "canvas" "caption" "cb" "cite" "code" "col" "colgroup" "css" "cssTable" "data" "datalist" "dd" "del" "details"
"dfn" "dialog" "div" "dl" "dt" "ele" "em" "fieldset" "figcaption" "figure" "footer" "form" "full" "h1" "h2" "h3"
"h4" "h5" "h6" "head" "header" "hgroup" "hr" "html" "i" "iframe" "img" "input" "ins" "kbd" "label" "legend" "li"
"link" "main" "map" "mark" "menu" "meta" "meter" "nav" "noscript" "object" "ol" "optgroup" "option" "output" "p"
"param" "picture" "pre" "progress" "q" "radio" "radioClear" "s" "samp" "script" "search" "section" "select" "selectClear"
"small" "source" "span" "strong" "style" "sub" "summary" "sup" "svg" "table" "tag" "tbody" "td" "template" "textarea"
"tfoot" "th" "thead" "time" "title" "tr" "track" "u" "ul" "var" "video" "wbr")
  "My yasnippet tags for HTML.")


;;;###autoload
(defun my-html-insert-yasnippet ()
  "Select and insert a yasnipppet key with completing-read."
  (interactive)
  (let ((completing-read-function #'ivy-completing-read)
        ;; dynamically shadow ivy completion style to ignore order.
        (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
        ;; taller ivy window. -4 so scrolling doens't go off screen.
        (ivy-height (- (window-height) 4)))
    (insert (completing-read "snippet: " my-html-yasnippets))))


;;; my-html-helpers.el ends here