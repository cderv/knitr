library(testit)

op = options(digits = 3, scipen = 0, knitr.digits.signif = TRUE)

assert('format_sci() uses correct number of significant digits', {
  (format_sci(1) %==% '1')
  (format_sci(0) %==% '0')
  (format_sci(3.1415e2) %==% '314')
  (format_sci(3.1415) %==% '3.14')
})

options(op)

op = options(digits = 14, scipen = 0, knitr.digits.signif = TRUE)
assert('format_sci() prints numerics at maximum number of significant digits', {
  (format_sci(3.14159265358979) %==% '3.1415926535898')
})
options(op)

op = options(digits = 4, scipen = 0)

assert('format_sci() turns numbers into scientific notations', {
  (format_sci(c(1.84e8, 1e5, 2.34e3)) %==% c('1.84\\times 10^{8}', '10^{5}', '2340'))
  (format_sci(1.23456789 * 10^-5) %==% '1.2346\\times 10^{-5}')
  (format_sci(10^-5) %==% '10^{-5}')
  (format_sci(10^5) %==% '10^{5}')
  (format_sci(-10^-5) %==% '-10^{-5}')
  (format_sci(-10^5) %==% '-10^{5}')
  (format_sci(9.87654e6, 'html') %==% '9.8765 &times; 10<sup>6</sup>')
  (format_sci(9.87654e6, 'rst') %==% '9.8765 |times| 10 :sup:`6`')
  (format_sci(letters) %==% letters)
  (format_sci(NA_real_) %==% NA_character_)
})

assert('format_sci() coerces non-numeric and non-double values to characters', {
  (format_sci(Sys.Date()) == as.character(Sys.Date()))
  (format_sci(1000000L) == '1000000')
})

# https://github.com/yihui/knitr/issues/1625
assert('format_sci() does not convert roman numerals to arabic numerals', {
  (format_sci(as.roman(c(1, 4, 7, 33, 100))) %==% c('I', 'IV', 'VII', 'XXXIII', 'C'))
})

assert('format_sci() for Rnw does not add \\ensuremath{} at all', {
  (!grepl('[\\]ensuremath', format_sci(c(1e4, 1.2345e10, 2 * pnorm(-(3:4)), -Inf))))
})

assert('the inline hook for Rnw applies \\ensuremath{} correctly', {
  (.inline.hook.tex(1e4) == '\\ensuremath{10^{4}}')
  (.inline.hook.tex(-Inf) == '\\ensuremath{-\\infty{}}')
  (.inline.hook.tex(-1.23) == '-1.23')
  (.inline.hook.tex(c(1.2345e10, 2 * pnorm(-(3:4)))) ==
    "\\ensuremath{1.2345\\times 10^{10}}, 0.0027, \\ensuremath{6.3342\\times 10^{-5}}")
})

assert('Infinity and NaN are formatted correctly', {
  (format_sci(-Inf) %==% '-\\infty{}')
  (format_sci(-Inf, 'html') %==% '-&infin;')
  (format_sci(-Inf, 'rst') %==% '-Inf')
  (format_sci(NaN) %==% 'NaN')
})

assert('sanitize_fn() warns against spaces in filenames', {
  (has_warning(sanitize_fn('figure/a b')))
})

options(op)

assert('fig_path() sanitizes paths', {
  (sanitize_fn('fig/foo') %==% 'fig/foo')
  (sanitize_fn('figure/a b  c', FALSE) %==% 'figure/a_b__c')
  (sanitize_fn('fig space/a.b') %==% 'fig_space/a.b')
  (sanitize_fn('../c.d') %==% '../c.d')
  (sanitize_fn('./../c..d') %==% './../c..d')
  (sanitize_fn('C:/foo/bar') %==% 'C:/foo/bar')
})

assert('fig_chunk() generates figure filenames for a code chunk', {
  (fig_chunk('foo') %==% 'figure/foo-1')
  (fig_chunk('foo', 'pdf') %==% 'figure/foo-1.pdf')
  (fig_chunk('foo', 'png', 2) %==% 'figure/foo-2.png')
  (fig_chunk('foo', 'svg', 1:5) %==% sprintf('figure/foo-%d.svg', 1:5))
  (fig_chunk('foo', fig.path = 'my_figure/') %==% 'my_figure/foo-1')
  (fig_chunk('foo', '.pdf') %==%'figure/foo-1.pdf')
})

assert('all_figs() generates all figure paths for a code chunk', {
  opts = list(fig.path = 'abc/', label = 'foo', fig.num = 3)
  (all_figs(opts, '.svg') %==% sprintf('abc/foo-%d.svg', 1:3))
  (all_figs(opts, c('png', 'pdf'))  %==% apply(
    expand.grid(1:3, c('.png', '.pdf')), 1, function(x) {
      paste0(c('abc/foo-', x), collapse = '')
    }
  ))
})

assert('escape_latex() escapes special LaTeX characters', {
  (escape_latex('# $ % & ~ _ ^ \\ { }') %==%
     '\\# \\$ \\% \\& \\textasciitilde{} \\_ \\textasciicircum{} \\textbackslash{} \\{ \\}'
  )
  (escape_latex('a b', spaces = TRUE) %==% 'a b')
  (escape_latex('a   b', spaces = TRUE) %==% 'a \\ \\ b')
})

assert('indent_block() works when the first element is empty (#790)', {
  (indent_block(c('', 'a')) %==% c('    ', '    a'))
  (indent_block(c('', '')) %==% c('    ', '    '))
})

assert('current_input() returns NULL by default', {
  (is.null(current_input()))
  (suppressWarnings(is.null(current_input(TRUE))))
})

assert('color_def() generates LaTeX code to define a color variable', {
  (color_def(NA) %==% '')
  (color_def('red') %==% '\\definecolor{shadecolor}{rgb}{1, 0, 0}')
  (color_def('#00ff00') %==% '\\definecolor{shadecolor}{rgb}{0, 1, 0}')
  (color_def('.5,.6,.7', 'fgcolor') %==% '\\definecolor{fgcolor}{rgb}{.5, .6, .7}')
})

opts = list(
  fig.cap = 'Figure "caption" <>.', fig.lp = 'Fig:', label = 'foo'
)
assert('.img.cap() generates the figure caption and alt attribute', {
  (.img.cap(list(fig.cap = NULL), FALSE) %==% "")
  (.img.cap(opts, FALSE) %==% opts$fig.cap)
  (.img.cap(opts, TRUE, TRUE)  %==% 'Figure "caption" &lt;&gt;.')

  opts$fig.alt = 'Figure "alternative text" <>.'

  (.img.cap(opts, TRUE, TRUE)  %==% 'Figure "alternative text" &lt;&gt;.')
  (.img.cap(opts, FALSE)  %==% opts$fig.cap)

  (.img.cap(list(fig.cap = '', fig.alt = "alt"), FALSE) %==% "")
  (.img.cap(list(fig.cap = '', fig.alt = "alt"), TRUE) %==% "alt")
})

z = as.strict_list(list(a = 1, aa = 2, bbb = 3))
assert('as.strict_list() does not allow partial matching', {
  (z$b %==% NULL)
  (z$bbb %==% 3)
})

out = c('*hello*', raw_output('<special>content</special> *protect* me!'), '*world*')
pre = extract_raw_output(out)
pre$value = gsub('[*]([^*]+)[*]', '<em>\\1</em>', pre$value)  # think this as Pandoc conversion
# raw output was protected from the conversion (e.g. *protect* was not converted)
assert('restore_raw_output() restores raw output', {
  (restore_raw_output(pre$value, pre$chunks) %==%
    '<em>hello</em>\n<special>content</special> *protect* me!\n<em>world</em>')
})

assert('raw_block() returns a raw attribute block for Pandoc', {
  (c(raw_latex('\\emph{x}')) %==% '\n```{=latex}\n\\emph{x}\n```\n')
  (c(raw_html('<i>foo</i>')) %==% '\n```{=html}\n<i>foo</i>\n```\n')
})

assert('block_class() turns a character vector into Pandoc attributes for code block classes', {
  (block_class(NULL) %==% NULL)
  (block_class('a') %==% 'a')
  (block_class('a', '.b') %==% '.a')
  (block_class('a b') %==% c('.a', '.b'))
  (block_class(c('a', 'b')) %==% c('.a', '.b'))
})

assert('when collapse is TRUE, class.* and attr.* become NULL except for class.source and attr.source', {
  keys = unlist(lapply(
    c('class.', 'attr.'), paste0, c('source', 'output', 'message', 'warning', 'error')
  ))
  keys_source = c('class.source', 'attr.source')
  opts = fix_options(opts_chunk$merge(c(setNames(as.list(keys), keys), list(collapse = TRUE))))
  (opts[keys_source] %==% as.list(setNames(keys_source, keys_source)))
  (!any(names(opts) %in% setdiff(keys, keys_source)))
  rm(keys, keys_source, opts)
})

assert('default strip.white is conditional to collapse', {
  opts = opts_chunk$get(default = TRUE)
  (fix_options(opts)$strip.white %==% TRUE)
  opts$collapse = TRUE
  (fix_options(opts)$strip.white %==% FALSE)
  opts$strip.white = TRUE
  (fix_options(opts)$strip.white %==% TRUE)
  rm(opts)
})

assert('pandoc_to gets the current Pandoc format', {
  opts = opts_knit$get(c('rmarkdown.pandoc.to', 'rmarkdown.pandoc.from'))
  opts_knit$delete(c("rmarkdown.pandoc.from", "rmarkdown.pandoc.to"))
  (pandoc_from() %==% 'markdown')
  (pandoc_to() %==% NULL)
  opts_knit$set(rmarkdown.pandoc.from = 'gfm', rmarkdown.pandoc.to = 'docx')
  (pandoc_from() %==% 'gfm')
  (pandoc_to() %==% 'docx')
  (pandoc_to('docx'))
  (!pandoc_to('pptx'))
  (pandoc_to(c('docx', 'pptx')))
  opts_knit$set(opts)
})

assert('comment_out() add prefix and newlines if asked', {
  (comment_out("a") %==% "## a\n")
  (comment_out("ab cd") %==% "## ab cd\n")
  (comment_out("ab cd\n") %==% "## ab cd\n")
  (comment_out("") %==% "## \n")
  (comment_out("\n") %==% "## \n")
  (comment_out(c("a", "b")) %==% c("## a\n", "## b\n"))
  (comment_out(c("a", "b"), which = 2) %==% c("a\n", "## b\n"))
  (comment_out("a", newline = FALSE) %==% "## a")
  (comment_out("a", prefix = "$") %==% "$ a\n")
  (comment_out("a", prefix = NULL) %==% "a\n")
})

assert('remove_urls() removes the link', {
  (remove_urls(c('[a](b)', '[a b](c)')) %==% c('a', 'a b'))
  (remove_urls('a [b](c) d.') %==% 'a b d.')
  (remove_urls('a [b](c) d [e f+g](h) i.') %==% 'a b d e f+g i.')
  (remove_urls('a [b](c) `[d](e)` f.') %==% 'a b `[d](e)` f.')
})

assert('options using `-` are converted to `.` and default value replaced', {
  opts = opts_chunk$merge(list('fig-cap' = 'caption', 'out-width' = 300))
  (is.null(dot_names(opts)[['fig-cap']]))
  (is.null(dot_names(opts)[['out-width']]))
  (dot_names(opts)[['fig.cap']] %==% 'caption')
  (dot_names(opts)[['out.width']] %==% 300)
  rm(opts)
})

assert('fig.format and fig.dpi', {
  opts = opts_chunk$merge(list('fig-format' = 'svg', 'fig-dpi' = 750))
  (is.null(dot_names(opts)[['fig-format']]))
  (is.null(dot_names(opts)[['fig-dpi']]))
  (dot_names(opts)[['dev']] %==% 'svg')
  (dot_names(opts)[['dpi']] %==% 750)
  rm(opts)
})

assert('options using `.` are converted to `-` and default value replaced', {
  opts = opts_chunk$merge(list('fig.cap' = 'caption', 'out.width' = 300))
  (is.null(dash_names(opts)[['fig.cap']]))
  (is.null(dash_names(opts)[['out.width']]))
  (dash_names(opts)[['fig-cap']] %==% 'caption')
  (dash_names(opts)[['out-width']] %==% 300)
  rm(opts)
})

assert('dev and dpi are convert to fig-format and fig-dpi', {
  opts = opts_chunk$merge(list('dev' = 'svg', 'dpi' = 750))
  (is.null(dash_names(opts)[['dev']]))
  (is.null(dash_names(opts)[['dpi']]))
  (dash_names(opts)[['fig-format']] %==% 'svg')
  (dash_names(opts)[['fig-dpi']] %==% 750)
  rm(opts)
})
