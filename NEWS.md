# CHANGES IN knitr VERSION 1.51

- Fix issue with error traceback not correctly showing when **rlang** is available.

- Improve error traceback when **rlang** is available and **evaluate** > 1.0.3 is used (thanks, @cderv, #2388).

- For Quarto, chunk options written in pipe comments must use the comment character of the chunk's language (#2225). Previously, comments written in `#|` will be parsed even if `#` is not the comment character of the chunk.

- `hook_optipng()` now uses the `-quiet` argument of `optipng` to suppress the messages by default.

# CHANGES IN knitr VERSION 1.50

## NEW FEATURES

- For inline code expressions, their specific line numbers will be shown in the message when errors occur (thanks, @kevinushey, #2387). Previously, the numbers were not specific to the inline code but the lines of the whole text chunk containing the inline code, which are often quite vague.

- Display error traceback when vignettes fail in `R CMD build` (thanks, @hadley, #2390).

- `kable()` properly supports column alignment for Org Mode tables now (thanks, @mclements, #2391).

## MINOR CHANGES

- Moved implementations of `combine_words()` and `write_bib()` to the **xfun** package as `xfun::join_words()` and `xfun::pkg_bib()`, respectively, since they are not directly relevant to **knitr**. The functions `combine_words()` and `write_bib()` are still kept in **knitr**, and can continue to be used in the future.

- A warning will be issued when chunk options are duplicated in both the chunk header and pipe comments (thanks, @cderv, #2386). A chunk option should appear in only one of these places.

- Removed the function `read_rforge()` since it has stopped working for a long time.

- Removed demos from the package (which were early experiments from several years ago).

# CHANGES IN knitr VERSION 1.49

## NEW FEATURES

- In-chunk references of the form `<<label>>` can be disabled via the chunk option `ref.chunk = FALSE` now (thanks, @jennybc @gadenbuie, #2360).

- Added support for `fig.alt` for LaTeX output, i.e., using `\includegraphics[alt={alt text}]` (thanks, @capnrefsmmat, #2378).

- The environment in which code chunks are evaluated can be changed by passing a custom environment to `knit_glbal()` now (thanks, @abhsarma, #2358).

## BUG FIXES

- In-chunk references of the form `<<label>>` should not be resolved if `label` is not found in the document (thanks, @jennybc @gadenbuie, #2360).

- The chunk option `autodep = TRUE` stopped working due to a regression from #2321 (thanks, @heavywatal #2344, @atusy #2377).

- `asis_output()` was not passed to the `output` hook (thanks, @cderv, #2332).

- Avoid partial matching of the `Date/Publication` field when generating `citation('knitr')`, otherwise R will emit a warning when `options(warnPartialMatchDollar = TRUE)` (thanks, @fkohrt, #2361).

## MAJOR CHANGES

- Unbalanced chunk delimiters (fences) in R Markdown documents are strictly prohibited now.

- For code chunks with `error = TRUE`, `purl()` and `hook_purl()` will wrap the code in `try({...})` (thanks, @bastistician #2338, @jeroen #2368).

## MINOR CHANGES

- If a character value is passed to the chunk option `message` or `warning`, it will be coerced by `as.logical()`, e.g., a character string `"NA"` will be coerced to `NA` (thanks, @cderv, #2375).

- Issue a warning when the chunk option `dependson` receives an invalid value (thanks, @otoomet, #2376).

- Changed the format of the reference card from PDF to HTML so building this package will not require LaTeX. See `vignette('knitr-refcard', package = 'knitr')`.

- Switched the vignette engine from **knitr** to `litedown::vignette` for some package vignettes.

# CHANGES IN knitr VERSION 1.48

## BUG FIXES

- Fix regression from 1.46 with `collapse = TRUE` option not correctly collapsing source code and output into one when code chunk returns multiple outputs (thanks, @jennybc, @florisvdh, tidyverse/reprex#463).

- `hook_purl()` should not write the path of the R script to the output document (thanks, @fenguoerbian, #2348).

# CHANGES IN knitr VERSION 1.47

## NEW FEATURES

- For `kable()`, you can set the global option `knitr.kable.max_rows` to limit the number of rows to show in the table, e.g., `options(knitr.kable.max_rows = 30)`. This is a way to prevent `kable()` from generating a huge table from a large data object by accident.

- `write_bib()` now escapes all non-escaped "&" in the bibliography by default. Previously, it only escaped the title field of the package citation. You can disable the escape with the argument `tweak = FALSE` (thanks, @HedvigS #2335, @atusy #2342).

## BUG FIXES

- Fixed a bug that `write_bib()` fails to use the first URL of a package when multiple URLs are provided in DESCRIPTION and separated by `\n` (thanks, @bastistician, #2343).

## MINOR CHANGES

- The syntax highlighting LaTeX commands for Rnw documents, `\hlstr` and `\hlstd`, were renamed to `\hlsng` and `\hldef`, respectively, to maintain consistency with Andrew Simon's highlight package (thanks, @dcser123, #2341).

# CHANGES IN knitr VERSION 1.46

## NEW FEATURES

- Added a new chunk option `tab.cap` to specify the table caption for `kable()` (thanks, @ulyngs, #1679). Previously, the caption could only be specified via the `caption` argument of `kable()`. Now you can set it in the chunk header if you want. Please note that this chunk option only works with a single `kable()` in each code chunk, and its value must be of length 1.

- `spin()` now recognizes `# %%` as a valid code chunk delimiter (thanks, @kylebutts, #2307).

- `spin()` also recognizes `#|` comments as code chunks now (thanks, @kylebutts, #2320).

- Chunk hooks can have the `...` argument now. Previously, only arguments `before`, `options`, `envir`, and `name` are accepted. If a chunk hook function has the `...` argument, all the aforementioned four arguments are passed to the hook. This means the hook function no longer has to have the four arguments explicitly in its signature, e.g., `function(before, ...)` is also valid if only the `before` argument is used inside the hook. See <https://yihui.org/knitr/hooks/#chunk-hooks> for more information.

- For package vignettes, PNG plots will be optimized by `optipng` and `pngquant` if they are installed and can be found from the system `PATH` variable. This can help reduce the package size if vignettes contain PNG plots (thanks, @nanxstats, <https://nanx.me/blog/post/rpkgs-pngquant-ragg/>).

## BUG FIXES

- `spin()` stopped working with input that cannot be parsed as R code due to #1605. Now it works again (thanks, @Hemken, #1773).

- `write_bib()` generated empty entries for packages without URLs (thanks, @bastistician, #2304).

- The `family` argument was not passed to the `pdf` device (thanks, @sebkopf, rstudio/rmarkdown#2526).

- Trailing spaces escaped by `\` should not be trimmed in `kable()` (thanks, @mjsmith037, #2308).

- `kable()` fails when the value of the `caption` argument is of length > 1 (thanks, @LeeMendelowitz, #2312).

- `include_graphics()` may provide an incorrect plot width to LaTeX when the locale setting for `LC_NUMERIC` is not `C` because the decimal separator may not be a dot (thanks, @tdhock, rstudio/rmarkdown#2525).

- When TinyTeX and the LaTeX package **pdfcrop** are installed, `knitr::pdf_crop()` is unable to find `pdfcrop` (thanks, @dmkaplan2000, rstudio/tinytex#435).

## MAJOR CHANGES

- Unbalanced chunk delimiters (fences) in R Markdown documents are no longer allowed, as announced two years ago at <https://yihui.org/en/2021/10/unbalanced-delimiters/> (#2306). This means the opening delimiter must strictly match the closing delimiter, e.g., if a code chunk starts with four backticks, it must also end with four; or if a chunk header is indented by two spaces, the closing fence must be indented by exactly two spaces. For authors who cannot update their R Markdown documents for any reason at the moment, setting `options(knitr.unbalanced.chunk = TRUE)` (e.g., in `.Rprofile`) can temporarily prevent **knitr** from throwing an error, but it is strongly recommended that you fix the problems as soon as possible, because this workaround will be removed in future.

- Package vignettes are tangled by default during `R CMD check`, per request from CRAN maintainers (d0d1b47). The consequence is that `R CMD check` will check R scripts tangled from vignettes by default, unless you set the environment variable `_R_CHECK_VIGNETTES_SKIP_RUN_MAYBE_=true`. Previously, **knitr** would skip tangling vignettes during `R CMD check`, because R scripts tangled from vignettes are not guaranteed to valid. With the skip undone, `R CMD check` may fail in places other than CRAN (because CRAN has set the environment variable).

## MINOR CHANGES

- Fixed broken vignettes, improved CSS for HTML vignettes, and reduced the file sizes.

- SQL code chunks that run `ALTER` statements are only executed and not tried to fecth a result (thanks, @maxschmi, #2330).

- The function `imgur_upload()` has been moved to (and enhanced in) the **xfun** package as `xfun::upload_imgur()` so it is no longer tied to **knitr** and can be reused by other pakages. Now `knitr::imgur_upload()` is only a wrapper function of `xfun::upload_imgur()`. You are recommended to use the latter (#2325).

- `spin()` dropped support for `#-` as the chunk delimiter token. Please use `#+` or `# %%` or `#|` instead.

- Faster processing of cache dependencies in `dep_auto()` (thanks, @knokknok, #2318).

- Removed some S3 methods that are used internally and changed them to normal functions: `print.block -> print_block`, `print.inline -> print_inline`, `process_group.block/process_group.inline -> process_group`, and `process_tangle.block/process_tangle.inline -> process_tangle`.

# CHANGES IN knitr VERSION 1.45

## NEW FEATURES

- Improved the error message to contain more specific information when YAML chunk options could not be parsed (thanks, @pedropark99, #2294).

## BUG FIXES

- Special characters in the chunk option `fig.alt` are properly escaped now (thanks, @jay-sf, #2290).

- Negative numbers returned from inline R expressions lost their minus signs when formatted in the scientific notation (thanks, @fkohrt, #2288).

- `convert_chunk_header(type = 'yaml')` will now use dash option name for known knitr options, and numeric option are kept with same significant digits, e.g `fig.width = 10` is converted to `fig-width: 10`.

- Add the necessary `\newline` to the last subfigure (thanks, @slrellison, rstudio/rmarkdown#2518).

- Percent signs (`%`) in LaTeX figure captions and short captions are properly escaped now (thanks, @s-u, #2302).

## MAJOR CHANGES

- The object `opts_current` will be restored after each code chunk has finished executing. Previously, it would not be restored, which means even for inline R expressions, `opts_current$get()` will inherit chunk options from a previous code chunk (thanks, @rundel, #1988). Besides, `opts_current$get('label')` will return a unique label for inline expressions. One possible application is to construct unique figure paths via `fig_path()` (e.g., ropensci/magick#310).

- `opts_current$set()` without `opts_current$lock(FALSE)` will trigger a warning instead of an error for now and it will become an error in future (#2296).

# CHANGES IN knitr VERSION 1.44

## NEW FEATURES

- `kable()` can generate Emacs org-mode tables now via `kable(..., format = 'org')` (thanks, @xvrdm #1372, @maxecharel #2258).

- Added support for the `qmd` (Quarto) output format to `spin()`, e.g., `spin('script.R', format = 'qmd')` (thanks, @cderv, #2284).

- `write_bib()` has a new argument `packageURL` to control whether to use a URL from the `DESCRIPTION` file or the one generated by `utils::citation()` (thanks, @dmurdoch, #2264).

- Updated the package vignette `vignette('knit_print', 'knitr')` to mention that package authors no longer have to make **knitr** a hard dependency if they want to define S3 methods for `knitr::knit_print` with R >= 3.6.0 (thanks, @cderv, #1929).

- Added a new function `download_image()` to download an image from a URL and include it via `include_graphics()`. This is mainly for including online images when the output format is not HTML (e.g., LaTeX), because the URL will not work as the image path, and it has to be downloaded beforehand (thanks, @bayeslearner, #2274).

## BUG FIXES

- Make the internal function `add_html_caption()` work with Quarto <= v1.3.353 (thanks, @giabaio, #2261).

- Fixed a bug in `spin(format = 'Rnw')` reported by @Tarious14 at https://github.com/yihui/yihui.org/discussions/769#discussioncomment-6587927

- When the chunk option `dev = 'svglite'`, the `svglite` device should be used to record plots (thanks, @Darxor, #2272).

- Figure captions are no longer escaped for reStructuredText output, and the alt text can also be specified via the `fig.alt` chunk option now (thanks, @trevorld, #2023).

- Use the correct type of progress bar when rendering Quarto documents in RStudio, which takes place in RStudio's background jobs pane or build pane (thanks, @hadley, #2271).

- The `opts_current` object can no longer be modified within code chunks via its `$set()` method (thanks, @AshesITR, #1798).

- The argument `col.names` of `kable()` can be used to specify the column name of row names now, e.g., `kable(head(mtcars), col.names = c("car", names(mtcars)))` (`"car"` will be the column name of row names in the first column) (thanks, @iago-pssjd, #1933).

## MAJOR CHANGES

- Dashes (`-`) in the names of all chunk options are normalized to dots (`.`) now, e.g., `fig-height` will be converted to `fig.height`. This is to make **knitr** more compatible with Quarto since Quarto always use dashes in chunk option names (#2282).

## MINOR CHANGES

- In-body chunk options (`#|`) are now preserved when extracting code from a document via `purl()` (thanks, @LuisLauM, #2268).

- A warning message will be issued when taking PDF screenshots for HTML widgets with the **webshot2** package, because **webshot2** doesn't use the correct figure size at the moment (thanks, @icejean, #2276).

- If the `title` argument of `knit2wp()` is omitted and a `title` field is specified in the YAML metadata of the input document, the YAML `title` will be used (thanks, @arencambre, #1924).

# CHANGES IN knitr VERSION 1.43

## NEW FEATURES

- Progress bar includes the chunk location (`chunk-name @ file:line`) when `options(knitr.progress.linenums = TRUE)` is set (thanks, @zeehio, #2232).

- The global option `knitr.progress.simple` can be used to decide whether to output the bar in the progress. When set to `FALSE`, only the step numbers and chunk labels will be printed, and the progress bar is omitted. This can be more useful for logging purposes since the bar itself is not useful (thanks, @hadley, #2221). By default, the simple progress output is used when the progress is not written to a connection such as `stdout` or `stderr` (e.g., written to a file instead), or the output connection is not a "terminal".

- HTML Widgets can now support alt text by specifying an attribute `aria-labelledby="<label>"` in their first HTML tag. The text will be obtained from the `fig.alt` or `fig.cap` chunk option in the usual way (thanks, @dmurdoch, #2243).

- Added a new argument `newline` to `kable()` to handle newlines in data when the table output format is Markdown-based (`simple`, `pipe`, `rst`, or `jira`). By default, newlines are not processed, which can result in broken tables. To substitute newlines with spaces, use `kable(..., newline = ' ')`. To remove newlines, use `kable(..., newline = '')` (thanks, @aronatkins, #2255).

## BUG FIXES

- The chunk option `collapse = TRUE` works with HTML widgets now (thanks, @dmurdoch, #2212).

- Option hooks should be run before child documents are processed (thanks, @richarddmorey, #2247).

- For `.Rnw` documents, `is_latex_output()` returns `TRUE` for output formats `sweave` (`render_sweave()`) and `listings` (`render_listings()`) now (thanks, @DavisVaughan, #2231).

- `write_bib()` does not fail anymore if an empty string is passed as package name (thanks, @phargarten2, #2240).

- Fix an issue with using `cache = TRUE` on `sql` engine chunk not defining a `output.var` (thanks, @mfherman, @eitsupi, #1842).

- `plot_crop()` correctly checks the required tools (`pdfcrop` and `ghostscript`) on Windows when the LaTeX is distribution is TeX Live or TinyTeX (thanks, @remlapmot, #2246). An external installation of `ghostscript` is no longer required on Windows, since TeX Live's built-in `ghostscript` will be used.

- The chunk option `dev.args` was not recognized in certain cases (thanks, @petrbouchal, #2238).

## MINOR CHANGES

- The `css` and `js` engines work for the `markdown` output format now. Previous these engines will not output anything when the output format is `markdown`. If you still want to disable them for `markdown` output, you may use the chunk option `eval = FALSE` or `eval = knitr::is_html_output(excludes = 'markdown')`.

- `is_html_output()` recognizes R Markdown v1 documents now (`.Rmd` documents compiled via the **markdown** package).

- For `.Rnw` documents, dots in figure file paths are no longer sanitized to underscores (thanks, @otoomet, #2213). Other special characters are still sanitized, but this feature can be turned off via `options(knitr.sanitize.paths = FALSE)`.

- `imgur_upload()` now recognizes a global option `knitr.imgur.key` or an environment variable `R_KNITR_IMGUR_KEY` for a custom client ID (thanks, @jonthegeek, #2233).

- `imgur_upload()` requires fewer package dependencies now. It only requires the **curl** package; **httr** is no longer required, and **xml2** has become optional.

# CHANGES IN knitr VERSION 1.42

## NEW FEATURES

- Better `.qmd` (Quarto) file support:
  - `purl()` will keep the in-chunk YAML syntax for options in the tangled R file
  - `knit()` will produce a `.md` file instead of `.txt`

- Users can specify a custom progress bar for `knit()` now. The default is still a text progress bar created from `utils::txtProgressBar()`. To specify a custom progress bar, set `options(knitr.progress.fun = function(total, labels) {})`. This function should take arguments `total` (the total number of chunks) and `labels` (the vector of chunk labels), create a progress bar, and return a list of two methods: `list(update = function(i) {}, done = function() {})`. The `update()` method takes `i` (index of the current chunk) as the input and updates the progress bar. The `done()` method closes the progress bar. See https://yihui.org/knitr/options/#global-r-options for documentation and examples.

- The default text progress bar is still written to `stdout()` by default, but can also be written to other connections or `stderr()` now. To do so, set `options(knitr.progress.output = )` to a connection or `stderr()`.

- Allow concordances to be embedded in HTML output (thanks, @dmurdoch, #2200).

## MAJOR CHANGES

- `knit()` no longer prints out chunk options beneath the text progress bar while knitting a document (thanks, @hadley, #1880).

- Due to a change in the **evaluate** package, the chunk options `message = FALSE` and `warning = FALSE` will completely suppress the messages/warnings now, instead of sending them to the console. To get back to the old behavior, you can use `NA` instead of `FALSE` (thanks, @gadenbuie, https://github.com/yihui/yihui.org/discussions/1458).

- The **stringr** dependency has been removed. All string operations are done with base R now (thanks, @HughParsonage #1549 #1552, @rich-iannone #2174 #2177 #2186 #2187 #2195 #2202 #2205).

## MINOR CHANGES

- Improved the error message when inline R code cannot be parsed (thanks, @hadley #2173, @rich-iannone #2198).

- If the chunk option `child` is used for a code chunk but the chunk is not empty, you will get a warning indicating that this non-empty code will not be executed when you knit the document. Now this warning can be silenced by `options(knitr.child.warning = FALSE)` (thanks, @jwijffels, #2191).

- Devices from the package **cairoDevice** (`Cairo_pdf`, `Cairo_png`, `Cairo_ps`, `Cairo_svg`) are no longer supported since the package has been archived on CRAN for more than a year (thanks, @jessekps, #2204).

## BUG FIXES

- The `sql` engine now correctly prints tables with `NA` in the first column (thanks, @mariusbommert, #2207).

# CHANGES IN knitr VERSION 1.41

## NEW FEATURES

- Added support for generating Jira tables via `kable(, format = 'jira')` (thanks, @pedropark99 #2180, @mruessler #2024).

- Added a new chunk option `fig.id`. When it is `TRUE`, the chunk option `out.extra` will gain an `id` attribute for each image from a code chunk in R Markdown (thanks, @jooyoungseo, #2169). By default, the attribute consists of a prefix, the chunk label, and the figure number. See <https://yihui.org/knitr/options/> for detailed documentation of `fig.id`.

- Added an argument `exact` to `pandoc_to()` and `pandoc_from()` to decide whether to use/return the exact Pandoc output/input format name. If not (default), Pandoc extensions will be removed from the format name, e.g., `latex-smart` will be treated as `latex`.

## BUG FIXES

- Plot created outside of `knit()` could sneak into `knit_child()` results (thanks, @niklaswillrich, #2166).

- User-provided background colors for code chunks in `.Rnw` documents may fail (thanks, @nielsrhansen, #2167).

- `tabularx` and `xltabular` tables should work without captions (thanks, @amarakon, #2188).

# CHANGES IN knitr VERSION 1.40

## NEW FEATURES

- Added a function `convert_chunk_header()` to convert the old in-header chunk options to the new in-body chunk options (#2149 #2151).

- Added a new "graphics device", `dev = "gridSVG"`, which uses `gridSVG::grid.export()` to export grid graphics to SVG (thanks, @jooyoungseo, #2152).

- Added a new engine `eviews`, which calls the **EviewsR** package to execute EViews code (thanks, @sagirumati, #2158).

- Added support for a `php` engine like other engines for interpreted languages. It will call `php -r <code>`, with `<code>` being the chunk content (thanks, @ralmond, #2144).

- Per suggestion of @jakubkaczor (#2116) and discussion with @pedropark99 (#2140), the chunk option `fig.sep` can also be used to add LaTeX code before the first sub-figure now. Previously this option can only be used for adding LaTeX code *after* each sub-figure.

- `knitr::kable()` supports `tabularx` and `xltabular` environments now for LaTeX tables, e.g., `knitr::kable(head(iris), format = 'latex', tabular = 'tabularx')` (thanks, @amarakon, #2138).

- For HTML output formats of R Markdown, SVG plots (e.g., in case of chunk option `dev = 'svg'` or `dev = 'gridSVG'`) can be embedded differently now when `options(knitr.svg.object = TRUE)`: if the HTML output is self-contained, the raw SVG code will be embedded directly in HTML, otherwise the `.svg` file is embedded in the `<object>` tag. By default, this feature is not enabled, i.e., the default is `options(knitr.svg.object = FALSE)` for backward-compatibility, which means the `<img>` tag is used for SVG plots just like other plot formats. This new feature will make assistive technology agents, such as screen readers, interact with SVG plots (thanks, @jooyoungseo, #2152).

## BUG FIXES

- Fixed `epub2` output not being considered as HTML format (thanks, @flipacholas, #2145).

- `kable(format = 'pipe')` calculates column widths correctly now if any text columns contain Markdown hyperlinks (thanks, @jacobbien, #2148).

- `dev.args = list(engine = ...)` was ignored by `dev = 'tikz'` when the tikz plot is compiled to PDF (thanks, @fkohrt, #2150).

## MINOR CHANGES

- When the inline R code cannot be correctly parsed, the error message will show the original code in addition to the parsing error, which can make it easier to identify the code error in the source document (thanks, @AlbertLei, #2141).

- The internal function `knitr:::wrap()` has been removed from this package. If you rely on this function, you will have to use the exported function `knitr::sew()` instead.

- Duplicate chunk label error will now be thrown with code chunks using `code` or `file` chunk options (thanks, @mine-cetinkaya-rundel, #2126).

- Simplified R Markdown's inline code parser (thanks, @atusy, #2143).

# CHANGES IN knitr VERSION 1.39

## MAJOR CHANGES

- Added an argument `rel_path` to `include_graphics()`, which defaults to `TRUE`, meaning that this function will try to convert absolute paths to relative paths automatically. If the conversion fails, it will issue a warning. If you want to suppress the conversion (and the warning), you may use `rel_path = FALSE` or set the global option `options(knitr.graphics.rel_path = FALSE)`. In the previous version of **knitr**, this function would always issue a warning when it detects absolute paths (thanks, @davidski @kendavidn, #2119).

## MINOR CHANGES

- For **knitr** source documents that generate `.tex` output documents (such as `.Rnw` and `.Rtex`), the LaTeX package **xcolor** will be used instead of **color** (#2085). This may cause option clashes if **xcolor** is already loaded by users with options, e.g., `\usepackage[dvipsnames]{xcolor}`. If this happens, you may set the package option `knitr::opts_knit$set(latex.options.xcolor = 'OPTIONS')` where `OPTIONS` is the options you used for **xcolor**, e.g., `dvipsnames`.

- The evaluation of chunk options written after `#|` using the YAML `!expr` syntax will be delayed until before the chunk is to be evaluated (#2120).

# CHANGES IN knitr VERSION 1.38

## NEW FEATURES

- The chunk option `file` can take a vector of file paths now, i.e., this option can be used to read more than one file (e.g., `file = c("foo.R", "bar.R")`.

- Added a new engine named `exec` (#2073) to execute an arbitrary command on the code chunk, e.g.,

  ````md
  ```{exec, command='Rscript'}
  1 + 1
  ```
  ````
  
  The above code chunk executes the `Rscript` command with the chunk body as its input (which basically means executing the R code in a new R session). See the example #124 in the repo https://github.com/yihui/knitr-examples for more info.
  
  There exists several command-based engines in **knitr**, such as `awk`, `bash`, `perl`, `go`, and `dot`, etc. This new `exec` engine provides a general mechanism to execute any command that users provide. For example, the code chunk
  
  ````md
  ```{bash}
  echo 'Hello world!'
  ```
  ````
  
  is equivalent to the chunk using the `exec` engine and the `bash` command:
  
  ````md
  ```{exec, command='bash'}
  echo 'Hello world!'
  ```
  ````
  
  With this new engine, we no longer need to provide or maintain other simple command-based engines. For example, to support TypeScript (https://github.com/yihui/knitr/pull/1833), we only need to specify `command = 'ts-node'` with the `exec` engine.
  
  If the command has significant side-effects (e.g., compile source code to an executable and run the executable, or generate plots to be included in the output), it is also possible to create a new engine based on the `exec` engine. The example #124 in the `knitr-examples` repo has provided a `gcc` example.
  
  We'd like to thank @TianyiShi2001 for the inspiration (#1829 #1823 #1833).

- Added a new engine `ditaa` based on the `exec` engine to convert ASCII art diagrams to bitmaps via [the `ditaa` command](https://github.com/stathissideris/ditaa) (thanks, @kondziu, #2092).

- Added two new chunk options, `class.chunk` and `attr.chunk`, for R Markdown output. These options can enclose the whole chunk output (source and results) in a fenced Div. Their syntax follows other chunk options with the similar names `class.*` and `attr.*` (e.g., `class.source` and `attr.source`). For example, `class.chunk = "foo"` would wrap the chunk output inside `::: {.foo}` and `:::` (thanks, @atusy, #2099).

- Added a new chunk option `lang` to set the language name of a code chunk. By default, the language name is the engine name. This is primarily useful for syntax highlighting the source chunks in Markdown-based output. Previously the `lang` option was only available to a few engines such as `verbatim` and `embed`. Now it is available to all engines.

- Added a new wrapper function `rnw2pdf()`. It allows users to specify an arbitrary output file path, clean the intermediate files, and stop when any error occurs in knitting (thanks, @shrektan, #2113).

- New `calling.handlers` option for `opts_chunk$set()` to register calling handlers within chunks. See `?evaluate::new_output_handler` for details.

## MAJOR CHANGES

- The minimal required version of R was bumped from 3.2.3 to 3.3.0 (thanks, @essemenoff, #2100).

- The working directory under which chunk options are evaluated has been changed to the directory of the source document by default. If the package option `root.dir` is set to a different directory, that directory will be used as the working directory (#2081).

- `include_graphics()` will expand `~` in the image paths now and also warn against absolute paths (thanks, @kcarnold, #2063).

- `opts_chunk$set()` returns values of old chunk options after setting new chunk options now, instead of returning `NULL`, which can make it a little simpler to reset chunk options, e.g., you can temporarily change a few chunk options and save them with `old = opts_chunk$set(error = FALSE, fig.height = 3)`, and reset them later with `opts_chunk$set(old)`. This works for any other objects in **knitr** that have the `$set()` methods, such as `opts_knit`, `opts_hooks`, `knit_hooks`, `knit_engines`, and so on.

## MINOR CHANGES

- The chunk option `fig.scap` has been added to `eval.after` in `opts_knit` (thanks, @knokknok, #2061).

## BUG FIXES

- Chunk options defined in the `#|` style are not recognized when the code chunk is indented or quoted (thanks, @mine-cetinkaya-rundel, #2086).

- Fixed a bug in `Sweave2knitr()` #2097 (thanks, @chroetz).

# CHANGES IN knitr VERSION 1.37

## NEW FEATURES

- Added a new chunk option named `file` so that the chunk content can be read from an external file. Setting the chunk option `file = "test.R"` is equivalent to using the chunk option `code = xfun::read_utf8("test.R")`.

- For R Markdown documents, code chunks can be embedded in a parent code chunk with more backticks now. For example, a code chunk with four backticks can contain code chunks with three backticks. One application is to conditionally include some verbatim content that contains code chunks via the `asis` engine, e.g.,

  `````md
  ````{asis, echo=format(Sys.Date(), "%w") == 1}
  Some conditional content only included when report is built on a Monday
  
  ```{r}
  1 + 1
  ```
  
  On another day, this content won't be included.
  ````
  `````
  
  Note that the embedded child code chunks (e.g., in the `asis` chunk above) are not parsed or evaluated by **knitr**, and only the top-level code chunk is parsed and evaluated.

- Added a new engine named `comment` to comment out content, e.g.,

  `````md
  ````{comment}
  Arbitrary content to be commented out.
  
  ```{r}
  1 + 1
  ```
  
  The above code chunk will not be executed.
  Inline code like `r pi * 5^2` will be ignored, too.
  ````
  `````
  
  Note that if any line of the content to be commented out contains `N` backticks, you will have to use at least `N + 1` backticks in the chunk header and footer of the `comment` chunk.

- Added a new engine named `verbatim` mainly for R Markdown documents to output verbatim content that contains code chunks and/or inline expressions, e.g.,

  `````md
  ````{verbatim}
  We can output arbitrary content verbatim.
  
  ```{r}
  1 + 1
  ```
  
  The content can contain inline code like
  `r pi * 5^2`, too.
  ````
  `````
  
  By default, the verbatim content is placed in a fenced `default` code block:
  
  `````markdown
  ````default
  We can output arbitrary content verbatim.
  
  ```{r}
  1 + 1
  ```
  
  The content can contain inline code like
  `r pi * 5^2`, too.
  ````
  `````
  
  You can change the `default` language name of the block via the chunk option `lang`, e.g., `lang = 'markdown'` will output a code block like this:
  
  `````markdown
  ````markdown
  We can output arbitrary content verbatim.
  
  ```{r}
  1 + 1
  ```
  
  The content can contain inline code like
  `r pi * 5^2`, too.
  ````
  `````
  
  To disable the language name on the block, use an empty string `lang = ''`.
  
  The difference between the `verbatim` and `asis` engine is that the former will put the content in a fenced code block, and the latter just output the content as-is.
  
  You can also display a file verbatim by using the chunk option `file`, e.g.,
  
  ````
  ```{verbatim, file="test.Rmd"}
  ```
  ````
  
  This engine also works for other types of documents (e.g., `Rnw`) but it will not allow for nested code chunks within the `verbatim` code chunk.

- Added a new engine named `embed` to embed external plain-text files. It is essentially a simple wrapper based on the `verbatim` engine, with the chunk content read from an external file and default language guessed from file extension. That is,

  ````
  ```{embed, file="foo.R"}
  ```
  ````
  
  is equivalent to
  
  ````
  ```{verbatim, file="foo.R", lang="r"}
  ```
  ````
  
  If you provide the chunk option `file` to the `embed` engine, it will read the file and show its content verbatim in the output document. Alternatively, you can specify the file path in the chunk body, e.g.,
  
  ````
  ```{embed}
  "foo.txt"
  ```
  ````
  
  The quotes are optional but can be helpful for editors (e.g., RStudio IDE) to autocomplete the file paths.
  
  The syntax highlighting language name is from the filename extension by default, and you can override it with the chunk option `lang` (e.g., `file = "foo.sh", lang = "bash"`) which is then identical to the `verbatim` engine.

## BUG FIXES

- The chunk option `child` also respects the package option `root.dir` now (thanks, @salim-b, https://community.rstudio.com/t/117563).

- Fixed a LaTeX error ``"Package xcolor Error: Undefined color `fgcolor'"`` with `.Rnw` documents (thanks, Kurt Hornik).

## MINOR CHANGES

- Improved the (warning) message when unbalanced chunk delimiters are detected in R Markdown documents, to make it easier for users to fix the problem. The message now contains the line numbers of the opening fence and closing fence, as well as the opening and closing backticks. For example, the opening fence may be `````"````{r}"````` (four backticks) but the closing fence is ````"```"```` (three backticks---should also be four to match the opening fence), or the opening fence is indented ````"  ```{r}"```` but the closing fence is not ````"```"````. Note that this warning message may become an error in the future, i.e., unbalanced chunk delimiters will no longer be allowed.

# CHANGES IN knitr VERSION 1.36

## MAJOR CHANGES

- In **knitr** 1.35, if the indentation of the closing backticks does not match the indentation of the chunk header in an Rmd document, the closing backticks would not be treated as closing fence of a code chunk. This behavior has been reverted because we have discovered several cases in which the indentation was accidental. A warning message will be issued instead, and you are still recommended to fix the improper indentation if discovered.

## BUG FIXES

- Fixed a regression in **knitr** 1.31 that caused package vignettes to generate (tangle) invalid R scripts (thanks, @t-kalinowski @halldc, #2052).

# CHANGES IN knitr VERSION 1.35

## NEW FEATURES

- Chunk options can also be written *inside* a code chunk now after the special comment `#|`, e.g.,

  ````
  ```{r}
  #| echo = FALSE, fig.width = 10,
  #| fig.cap = "This is a long caption."
  
  plot(cars)
  ```
  ````
  
  The main differences between this new syntax and traditional syntax (i.e., chunk options in the chunk header) are: 1) the chunk options can be freely wrapped, i.e., you can write them on as many lines as you prefer; 2) you can also use the YAML syntax instead of the comma-separated syntax if you like, e.g.,
  
  ````
  ```{r}
  #| echo: false
  #| fig.width: 10
  ```
  ````
  
  Chunk options provided inside a code chunk will override options with the same names in the chunk header if chunk options are provided in both places, e.g.,
  
  ````
  ```{r, echo = TRUE}
  #| echo = FALSE, fig.width = 10
  ```
  ````
  
  The effective chunk options for the above chunk are `echo = FALSE` and `fig.width = 10`.

## MAJOR CHANGES

- For R Markdown documents, if the chunk header is indented, the closing backticks (usually ```` ``` ````) of the chunk must be indented with the same amount of spaces (thanks, @atusy, #2047). For example, the following is no longer a valid code chunk because the chunk header is indented but the closing backticks are not:

  ````
   ```{r}
  1 + 1
  ```
  ````
  
  If you see an error "attempt to use zero-length variable name" when knitting an Rmd document, it may be because of this change, and you may have indented the chunk header by accident. If that is the case, you need to remove the extra white spaces before the chunk header.
  
  The same problem applies to blockquotes, i.e., `>` before ```` ``` ````. If you quote the chunk header, you have to quote the footer as well, e.g.,
  
  ````
  > ```{r}
  1 + 1
  ```
  ````
  
  The above unbalanced code chunk needs to be corrected to:
  
  ````
  > ```{r}
  > 1 + 1
  > ```
  ````
  
  Quoting the chunk body is optional but recommended.

## BUG FIXES

- Fixed a regression in v1.34: now blank lines in code chunks are stripped only when `collapse = FALSE` but no longer stripped by default when `collapse = TRUE`. If you prefer blank lines to be always stripped, set `strip.white = TRUE` globally or on the per chunk basis (thanks, @IndrajeetPatil, rstudio/rmarkdown#2220, #2046).

- In `knitr::combine_words()`, when `words` is length 2 and `and = ""`, `sep` will now be used (thanks, @eitsupi, #2044).

- For R Markdown documents, if the chunk output contains N backticks, the `output` hook will use N + 1 backticks to wrap the output, so that the N verbatim backticks can be correctly preserved (thanks, @atusy, #2047).

# CHANGES IN knitr VERSION 1.34

## NEW FEATURES

- Added a package option `latex.tilde` so that users can customize the tilde symbol, e.g., you can set `knitr::opts_knit$set(latex.tilde = "\\hlopt{\\scriptsize{\\ensuremath{\\sim}}}")`. This is because the default tilde is too high in the PDF output, and it's often more desirable to use a middle tilde instead (thanks, @brry #1992, @jaredlander #492).

- For the `tikz` engine, the class options of the `standalone` document classs can be specified via the chunk option `engine.opts$classoption` (thanks, @XiangyunHuang, #1985). The default value is `tikz`, i.e., `\documentclass[tikz]{standalone}` is used by default.

- Added the ability to pass additional arguments to `dvisvgm` when using the `tikz` engine with `fig.ext = "svg"` by using `dvisvgm.opts` in `engine.opts` (thanks, @andrewheiss, #2039). Recent versions of `dvisvgm` now allow you to embed fonts into SVG files as base64-encoded WOFF files, so `tikz` chunks can embed fonts using like so: ```` ```{tikz, fig.ext="svg", engine.opts=list(dvisvgm.opts = "--font-format=woff")}````.

- Added a new `targets` engine (https://github.com/ropensci/targets/issues/503, @wlandau). Details: https://books.ropensci.org/targets/markdown.html.

- The [chunk option `cache.globals`](https://yihui.org/knitr/options/) can take values `TRUE` and `FALSE` now (in addition to a character vector). When `FALSE`, it tries to find all symbols in the code chunk, no matter if they are local or global variables. When `NULL` or `TRUE`, it tries to find all global variables (thanks, @knokknok, #1898).

## MAJOR CHANGES

- An error is now thrown when an inline code result is not coercible to character. This has always been the assumed behavior but it happens to be different in certain formats with unknown automatic coercion. This is now enforced to prevent any unexpected output. An inline code expression must evaluate to a character vector or an object coercible by `as.character()` (#2006).

- The **markdown** package has been moved from `Imports` to `Suggests` in **knitr**'s `DESCRIPTION`, which means it is no longer a hard dependency but has become a soft dependency of **knitr** (#1864). One consequence for package developers is that if you use the vignette engines based on **markdown** (such as `knitr::knitr`), you will have to explicitly declare the (soft) dependency on **markdown**, because the implicit dependency through **knitr** is no longer there.

- For R packages that use the `knitr::rmarkdown` engine to build their vignettes, `rmarkdown` must be declared as a dependency in the package `DESCRIPTION` (e.g., in `Suggests`). Previously, **knitr** would fall back to using the **markdown** package to build vignettes if **rmarkdown** is not declared (#1864, #2020).

- `write_bib()` only uses the first URL if multiple are found in a package (#2028).

## MINOR CHANGES

- The attribute `data-external="1"` will be added to `<iframe>`s generated by `include_url()` to prevent Pandoc from embedding the URL as base64 data (thanks, @IndrajeetPatil, https://stackoverflow.com/q/67477667/559676).

- The chunk option `strip.white = TRUE` used to work only when the chunk option `collapse = FALSE`. Now the two options are independent, i.e., `strip.white` also works when `collapse = TRUE` (thanks, @kbvernon, #2011).

- When building R Markdown vignettes but Pandoc is not available, the vignette engine will emit a message instead of a warning before falling back to using the **markdown** package.

- The internal function `is_abs_path()` has been removed. Users (if any) should use the exported function `xfun::is_abs_path()` instead.

## BUG FIXES

- Fix an issue with the RStudio IDE when using `knitr::include_url()` or `knitr::include_app()` in interactive Notebook mode. This will no more cause an error but print the list object as is (thanks, @systemnova, #2015).

- Fix a regression with `fig.keep` chunk option used in chunks with only one figure where the figure was not showing in output (thanks, @fmichonneau, #1993).

- Allow vignettes to be tangled (and output compared) in `R CMD check` if they have a corresponding `.Rout.save` (thanks, @lentinj, #2018).

# CHANGES IN knitr VERSION 1.33

## NEW FEATURES

- Exported the internal functions `sew()` (previously named `wrap()`) and `is_low_change()` to make it possible for other graphics systems such as **rgl** to work in **knitr** like base and grid graphics in base R (thanks, @dmurdoch, #1892 #1853).

- For `sql` code chunks, parameterized queries will be executed through native database api if the chunk option `params` is provided (thanks, @byapparov, #1987).

## BUG FIXES

- Reverted the fix for #1595 since it caused problems in **kableExtra** (thanks, @bttomio, haozhu233/kableExtra#607), and applied a different fix to the original problem (i.e., add `{}` before `[`).

## MAJOR CHANGES

- The internal S3 generic function `wrap()` has been renamed to `sew()`.

## MINOR CHANGES

- For package vignettes that use the vignette engine `knitr::docco_linear` or `knitr::docco_classic`, **knitr** will signal an error during `R CMD build` if the **markdown** package is not declared as a soft dependency in the `Suggests` field in `DESCRIPTION`. The same treatment has been applied in the previous version of **knitr** for the vignette engine `knitr::knitr` (#1864).

- `R CMD build` will signal an error when a package uses R Markdown vignettes to be build with the **rmarkdown** package but the **rmarkdown** package is not installed (thanks, @nsheff, #1864).

- The `force_v1` argument of `knit2html()` defaults to `getOption('knitr.knit2html.force_v1', FALSE)` now, which means its value can be configured via a global option.

# CHANGES IN knitr VERSION 1.32

## NEW FEATURES

- The chunk option `ref.label` can be used to reuse code from other code chunks in the current code chunk. Now it can also accept a character vector of chunk labels wrapped in `I()` so that the chunk options of other code chunks can also be applied to the current chunk. For example, if we have a chunk ```` ```{r, chunk-a, echo=FALSE, results='asis'}````, the chunk ```` ```{r, chunk-b, ref.label=I('chunk-a')}```` will inherit both the code and chunk options from `chunk-a` (e.g., `echo=FALSE` and `results='asis'`), but `chunk-b` can still override these chunk options, e.g., the actual chunk options of ```` ```{r, chunk-b, echo=TRUE}```` will be `echo=TRUE` and `results='asis'`. If you want to use one code chunk with several chunk options for multiple times, and each time you want to change a small subset of the chunk options, you may use this approach `ref.label = I()` to avoid typing most chunk options over and over again (thanks, @atusy, #1960).

- The chunk option `opts.label` gained a new usage. It used to mean labels in `knitr::opts_template` (see the help page if you are not familiar with this object). Now it can also be used to specify the labels of other code chunks, from which the chunk options are to be inherited to the current chunk. For example, ```` ```{r, chunk-b, opts.label='chunk-a'}```` means that `chunk-b` will inherit chunk options from `chunk-a`. Local chunk options in a chunk (if exist) will override inherited options. For the new features about both chunk options `ref.label` and `opts.label`, see the detailed documentation and examples at https://yihui.org/knitr/options/ (thanks, @atusy, #1960).

- The internal code to process R code chunks was factored out into the language engine `R`, which can be obtained via `knit_engines$get('R')` now (thanks, @abhsarma, #1963).

- Added arguments `dir` and `envir` to `load_cache()` to specify the working directory and environment into which the cached objects are loaded. By default, all cached objects are loaded into the global environment, which may not be desirable (thanks, @LTLA, #1905).

- The internal function `html_screenshot()`, which takes screenshots of HTML widgets and Shiny apps in **knitr** documents, now prefers the **webshot2** package over the **webshot** package. This is because the backend of the **webshot** package is PhantomJS, which [has been archived since 2018](https://github.com/ariya/phantomjs/issues/15344). If **webshot** is still preferred, use the chunk option `opts_chunk$set(webshot = "webshot")` (thanks, @atusy #1918, @LouisStAmour #1858).

- Added a new engine, `bslib`, which is a special `scss`/`sass` engine for writing [Sass](https://sass-lang.com) code that wants to leverage [Bootstrap Sass](https://getbootstrap.com/docs/4.6/getting-started/theming/). It's primarily designed to work with `rmarkdown::html_document_base`'s `theme` parameter (when it represents a `bslib::bs_theme()`), but the engine can be used anywhere a `bslib::bs_theme()` is set globally with `bslib::bs_global_set()` (thanks, @cpsievert, #1666).

- When the chunk option `include = FALSE`, `error = TRUE` used to be ignored, i.e., errors are signaled nonetheless (causing the knitting process to fail). To avoid this problem, you can now use the numeric value `error = 0` so that **knitr** will not check the `include` option, i.e., knitting will not fail even if `include = FALSE` and an error has occurred in a code chunk, which you wouldn't notice (thanks, @rundel, #1914).

- When processing package vignettes that require **rmarkdown** or **markdown**, we now check that it is declared as a package dependency in the `DESCRIPTION` file (thanks, @dmurdoch, #1919).

- For `tikz` code chunks, a new option `engine.opts$extra.preamble` allows arbitrary LaTeX code to be inserted into the preamble of the template. This allows loading of additional LaTeX and tikz libraries without having to recreate a template from scratch (thanks, @krivit, #1886).

## BUG FIXES

- Fixed an issue where code source and results would not show when using a numeric vector in `fig.keep` to select all plots (thanks, @dongzhuoer @ajrgodfrey #1579, @cderv #1955).

- Graphical parameter `par(col =)` is now preserved as expected in following chunks when `knitr::opts_knit$set(global.par = TRUE)` (thanks, @ArnonL, @RolandASc, #1603).

- Fixed an issue with `kable(format = "latex", booktabs = TRUE)` when the first cell of the table stats with `[` (thanks, @jonnypenarth, #1595).

- For child documents, `include_graphics(error = FALSE)` is the default now (#1957).

- For Rnw documents, the commented `%\begin{document}` will no longer cause trouble (thanks, @NewbieKnitter @shrektan, #1819).

- Fixed an issue with the chunk option `fig.alt` causing figures to disappear in `rmarkdown::word_document()` output. This option is currently supported for HTML output only. If provided for office output in **rmarkdown**, it will emit a warning and be ignored (#1966).

- Spaces in messages were not properly escaped in `.Rnw` documents (thanks, @elbersb, #1978).

- The hook `hook_pdfcrop()` did not crop figures generated by the `tikz` or `dot` engine (thanks, @giabaio yihui/xaringan#285, @cderv #1923).

## MINOR CHANGES

- For Rnw documents, if a chunk's output ends with `\n`, **knitr** will no longer add another `\n` to it (thanks, @krivit #1958, @jpritikin 1092).

- For `kable(x, format = 'simple')`, it no longer generates a `pipe` table but a `simple` table instead when `x` has only one column (thanks, @alusiani, #1968). For `kable(x, format = 'pipe')`, it no longer warns when `x` has no column names, but just generate an empty table header.

- For `tikz` code chunks, the default LaTeX template uses `\documentclass[tikz]{standalone}` instead of `\documentclass{article}` and `\usepackage{preview}` now (thanks, @XiangyunHuang, #1985).

# CHANGES IN knitr VERSION 1.31

## NEW FEATURES

- Exported the functions `pandoc_to()` and `pandoc_from()` to get the output and input Pandoc format respectively when knitting R Markdown documents (thanks, @cderv, #1951).

- Added a new chunk option `fig.alt` for users to specify the alternative text in the `alt` attribute of the `<img>` tag  (images). Previously, the `alt` attribute takes value from the chunk option `fig.cap` (it still does so if `fig.alt` is `NULL`). If there are multiple plots/images in a chunk, you can pass a character vector to `fig.alt`, and it will be applied to individual images (thanks, @cderv, #1900).

- `include_url()` and `include_app()` can now take a vector of URLs (thanks, @cderv, #1948).

- The `sql` engine now correctly captures error with the chunk option `error = TRUE` (thanks, @colearendt, rstudio/rmarkdown#1208).

- The chunk option `collapse = TRUE` now works as expected when the chunk option `attr.*` or `class.*` is provided. By this change, The chunk option `collapse = TRUE` forces `attr.*` and `class.*` be `NULL` except for the chunk options `attr.source` and `class.source` (thanks, @aosavi @cderv @atusy, #1902 #1906).

- New links added in `?knitr::kable()` help page to the Rmarkdown Cookbook relevant pages. 

- The table label can be disabled via `kable(label = NA)` now (thanks, @NickCH-K, #1937).

- The `table.attr` argument in `kable(.., format = 'html')` can take the value from the global option `knitr.table.html.attr` now (thanks, @cderv, #1922).

- Added a new argument `oxford_comma` to the function `combine_words()` (thanks, @thompsonsed, #1946).

- The label prefix of tables generated by `kable()` can be customized via the package option `label.prefix` in `opts_knit` now. This option takes a named character vector, and the default is `knitr::opts_knit$set(label.prefix = c(table = 'tab:'))`. If you want to use a different prefix such as `tbl`, you may set the option to `c(table = 'tbl:')` (thanks, @luispfonseca, #1890).

- The global option `options(knitr.device.fallback = TRUE)` can be used to allow the graphical device specified in the chunk option `dev` to fall back to other usable devices if the specified device is not operational. Users can provide a list of fallback devices via the global option, e.g., `options(knitr.device.choices = list(png = c('jpeg', 'svg'), jpeg = c('tiff')))`, which means the `png` device can fall back to `jpeg` and `svg` (the first operational device in the list is used) and `jpeg` can fall back to `tiff`.

## BUG FIXES

- Line numbers in error messages are incorrect when the error occurs in a child Rmd document (thanks, @cderv @khughitt, #1945).

## MAJOR CHANGES

- Previously `knit_exit()` in a child document would not terminate the knitting process of its parent document. Now it will terminate the whole process by default. You can use `knit_exit(fully = FALSE)` to exit the child document only (thanks, @davidchall, #1810).

## MINOR CHANGES

- Vignette engines for R Markdown vignettes no longer check for availability of `pandoc-citeproc` since it has gone since Pandoc 2.11.

# CHANGES IN knitr VERSION 1.30

## NEW FEATURES

- Added `knitr::hooks_*()` functions to get a list of output hooks for a specific format. Previously, these hooks only exist inside the `knitr::render_*()` functions, and users do not have direct access to them. Now they can be accessed directly, e.g., via `knitr::hooks_markdown()` to get a list of output hooks for R Markdown documents. You can also set the output hooks individually, e.g., `knitr::knit_hooks$set(knitr::hooks_markdown()['source'])` only sets the _source_ ouput hook. See more on output hooks at https://yihui.org/knitr/hooks/#output-hooks and https://bookdown.org/yihui/rmarkdown-cookbook/output-hooks.html (thanks, @cderv, #1889).

- Added an argument `lib.loc` to `knitr::write_bib()`.

## BUG FIXES

- The option `fig_caption = FALSE` for `rmarkdown::html_document()` was unable to suppress the figure captions in some cases.

- The internal function `fix_options()` should be called after option hooks are executed (thanks, @atusy, #1876 and #1877).

- When the option `options(OutDec = )` is set to a value other than `"."`, percentages in the chunk options `out.width` and `out.height` do not work (thanks, @omgitsmoe, #1887).

## MINOR CHANGES

- `knitr::write_bib()` removes pairs of single quotes in the titles of citation entries now.

- `knitr::write_bib()` uses the `URL` in the package `DESCRIPTION` if it is provided, instead of the canonical CRAN URL for the package.

- `hook_pdfcrop()` and `plot_crop()` will work only when both programs `pdfcrop` and `ghostscript` have been installed. Previously only `pdfcrop` was checked (thanks, @dalupus, #954).

# CHANGES IN knitr VERSION 1.29

## NEW FEATURES

- Added a new function `kables()` (the plural form of `kable()`) to create a table containing multiple tables, with each table created by `kable()`. Previously you can only create such a table from `kable(list(data1, data2))`, and now you can also do `kables(list(kable(data1), kable(data2)))`. The latter way gives you more freedom to control each table individually via the arguments of each individual `kable()` call.

- The graphics device chunk option, `dev`, now supports a value of `'ragg_png'` which calls the `agg_png()` function from the **ragg** package (thanks, @cpsievert, #1834).

- The argument `error` of `include_graphics()` takes value from the global R option `knitr.graphics.error` by default, e.g., you may set `options(knitr.graphics.error = FALSE)` so `include_graphics()` won't signal an error if the graphics file to be included doesn't exist.

- Added a new engine `cc` to compile C++ code via `R CMD SHILIB` like the `c` and `fortran` engines (thanks, @kingaa, #1832).

## BUG FIXES

- For non-R code chunks that generate plots and use the chunk option `fig.cap`, the `plot` hook in **knitr** stopped working in v1.27 and v1.28 (thanks, @XiangyunHuang, https://d.cosx.org/d/421249).

- The `sql` engine fails when the first column of the database is of the type `bit64::integer64` (thanks, @randy3k, #1837).

- The chunk option `dpi` is correctly passed to `showtext::showtext_opts()` now (thanks, @cpsievert, yixuan/showtext#33).

- Theorem environments in **bookdown** fail to work with Pandoc >= 2.7.3 because of an issue in the `block2` engine of **knitr** (rstudio/bookdown#883).

## MAJOR CHANGES

- For R Markdown documents, the default table format generated by `kable()` was changed from Pandoc's simple tables to pipe tables. This fixes bugs #1788 (`kable()` fails to align columns containing wide characters) and #1817 (broken column headers) (thanks, @atusy, #1830).

- When a code chunk generates multiple plots to be placed in figure environments, their figure labels will be of the form `label-i` where `label` is the chunk label, and `i` is the plot number. Previously, the labels were of the form `labeli` (i.e., there was not a dash before the number). If you cross-reference the figures, you need to change the reference keys from `labeli` to `label-i`.

- For a non-matrix object passed to `kable()`, its columns are formatted individually via `format()`. Previously, the whole object is coerced via `as.matrix()` (thanks, @thebioengineer, #1827).

- For packages that contain R Markdown vignettes using the vignette engines based on the **markdown** package (e.g., `knitr::knitr`), the **markdown** package should be declared in `Suggests` in the package `DESCRIPTION` unless it has already been declared as a dependency in other fields. For this version of **knitr**, you will see a warning in `R CMD check` in the CRAN incoming check. In the future, you may see a warning in a regular `R CMD check`, so please add **markdown** to `Suggests` at your earliest convenience. If it is not clear how to do it, please see #1864.

## MINOR CHANGES

- `knitr::image_uri()` calls `xfun::base64_uri()` instead of `markdown:::.b64EncodeFile()` now.

- `knitr::write_bib()` now adds an empty line after each BibTeX entry (#1862).

# CHANGES IN knitr VERSION 1.28

## BUG FIXES

- `hook_pdfcrop()` no longer crops images included via `include_graphics()` now (thanks, @hpages #1797, @salim-b #1800).

## MAJOR CHANGES

- If the `hook_pdfcrop()` is enabled, the non-PDF image will be cropped only if the **magick** package is available, otherwise it will signal a message. In the previous version, it will signal an error (thanks, @trannhatanh89, #1796).

- By default, `include_graphics(files)` will signal an error if any `files` do not exist and are not web resources. To avoid the error (e.g., if you think it is a false positive), use `include_graphics(..., error = FALSE)` (thanks, @hadley, #1717).

# CHANGES IN knitr VERSION 1.27

## NEW FEATURES

- The chunk option `message = FALSE` can now be used to suppress the message "running: command ..." when the language engine is a command such as `bash`, `awk`, `perl`, `ruby`, `sas`, `sed`, and `stata`, etc. (thanks, @splaisan, #1782).

## BUG FIXES

- The figure caption was placed incorrectly when there are multiple `include_graphics()` in a code chunk (thanks, @hadley #1771, @cderv #1776).

- The chunk option `fig.keep` was buggy when taking a numeric vector. The bug was originally introduced in #1265 and discovered in the SO post https://stackoverflow.com/q/59180351/559676.

- When the chunk option `dev = 'tikz'` and a plot in the code chunk is generated by **ggplot2** with a legend on a continuous scale, **tikzDevice** will create a raster image for the legend. Now the path to the raster image in the `.tex` file is tweaked to include the dir name of the `.tex` file, so that the `.tex` file can be correctly compiled to PDF via LaTeX (thanks to @rstub for the debugging at https://stackoverflow.com/a/58410965/559676).

- `hook_pdfcrop()` does not crop all plots when multiple graphical devices are used in a code chunk (i.e., the chunk option `dev` takes a vector of devices) due to a bug in the internal function `all_figs()` identified by @bastistician in #1766, who also proposed a fix.

## MAJOR CHANGES

- `options('width')` no longer affects caching. With this change, your previous cache will be invalidated if you update **knitr** from 1.26 to 1.27. If you prefer that changes in the R global option `width` invalidate cache (as in previous versions of **knitr**), you may associate it with a chunk option, e.g., `cache.extra = getOption('width')`. The reason for this change is that it is too costly to invalidate the cache when the `width` option changes---the effect of this option is only cosmetic and the code chunk output may not really rely on this option (thanks, @jaburgoyne, #1781).

## MINOR CHANGES

- `is_html_output()` returns `TRUE` for the Pandoc output format `gfm` now (thanks, @ttimbers @cderv, rstudio/rmarkdown#1756).

- `plot_crop()` no longer calls the `convert` command in ImageMagick to trim non-PDF plots. Instead, the function `magick::image_trim()` is called. This means you no longer need to install ImageMagick, but should install the R package **magick** (thanks, @hpages, #1785).

- When the chunk option `fig.scap` is used in R Markdown and the output format is LaTeX/PDF, the plot hook `knitr::hook_plot_tex()` will be used to create the appropriate figure environment. In previous versions, `fig.scap` is silently ignored (thanks, @billdenney, #1793).

# CHANGES IN knitr VERSION 1.26

## NEW FEATURES

- `write_bib()` also includes all citation entries in the package's `CITATION` file if provided.

## MINOR CHANGES

- `knit()` no longer adjusts the global R option `width`; previously it sets `options(width = 75)` by default.

# CHANGES IN knitr VERSION 1.25

## NEW FEATURES

- Office outputs (e.g., `rmarkdown::word_document`) supports the following chunk options `out.width`, `out.height`, `out.extra`. Their behavior follows the behavior of Pandoc's `link_attributes` extension (thanks, @atusy, #1746).

- The chunk option `fig.process` can be specified by a function with the `options` argument to read chunk options (thanks, @atusy, #1749).

- For `kable(format = 'latex')`, a single string consisting of `l`, `c`, and `r` for the `align` argument also works now, e.g., `knitr::kable(head(iris), 'latex', align = 'llcrr')`. Previously it has to be a character vector of individual letters.

## MAJOR CHANGES

- Reverted #1519 (which intended to fix #1487): the `valign` argument of `kable(format = 'latex')` meant to control the vertical alignment of the inner `tabular` environment, instead of the floating position of the outer `table` environment. A separate argument named `position` has been added to control the floating position (e.g., `position = "!b"`).

## MINOR CHANGES

- The returned value of `combine_words()` is wrapped in `xfun::raw_string()` for pretty printing.

## BUG FIXES

- The `output` hook for `render_sweave()` failed to respect the chunk option `results='asis'` (thanks, Achim Zeileis, https://stackoverflow.com/q/57571790/559676).

- Added the unit `px` to the chunk option `out.width` if its value is numeric (thanks, @chendaniely, #1761).

- The chunk option `dependson` did not work for non-R engines (thanks, @nielsrhansen, #1601).

# CHANGES IN knitr VERSION 1.24

## MAJOR CHANGES

- The `input` argument of `knitr::knit()` must be a file path (a character string). It can no longer be connections.

- The `encoding` argument of `knitr::knit()` is ignored. The encoding is always assumed to be UTF-8. Please see https://yihui.org/en/2018/11/biggest-regret-knitr/ for more info.

## BUG FIXES

- Inline code chunk references are parsed as character vectors instead of single character strings separated by `\n` now. This will avoid errors when a python code chunk contains a reference to a code chunk that contains multiple statements (thanks, @Atrebas, rstudio/rmarkdown#1589).

# CHANGES IN knitr VERSION 1.23

## NEW FEATURES

- Added `mogrify` support in `hook_png()` and `hook_mogrify()` in order to obtain trimmed `.png` files (thanks, @mhofert, #1697).

- It is possible to customize the sign `\times` used for the scientific notation of inline numeric output via a global option, e.g., `options(knitr.inline.times = '\\cdot ')` (thanks, @wuffi @trentks, #1563).

- Exported `knit_code` and documented it at `?knitr::knit_code`. Here be dragons, but I know you are brave and creative (thanks, @r-cheologist @ProQuestionAsker, #1545).

- Added support for specifying fenced code block attributes in Markdown output via chunk options `attr.*`, which are similar but a generalization to the existing chunk options `class.*`. For example, `class.source = 'numberLines'` is equivalent to `attr.source = '.numberLines'`, but `attr.source` can take arbitrary attribute values, e.g., `attr.source = c('.numberLines', 'startFrom="11"')` (thanks, @atusy, #1710).

## BUG FIXES

- For R Markdown documents, inline numeric output that is formatted with the scientific notation will be wrapped in `\ensuremath{}`. This will fix the LaTeX error `! Missing $ inserted.`. However, you are still recommended to add a pair of dollar signs around the inline output when it is formatted with the scientific notation (e.g., `` $`r 1.234e12`$``), otherwise it will not work with other R Markdown output formats such as HTML or Word (thanks, @billdenney and many who have suffered from this issue, rstudio/rmarkdown#1527).

- For an input file that contains no code chunks or inline R code expressions, `purl()` should return an empty string instead of the original text from the input (thanks, @jrnold, #1660).

- `purl()` might trigger a warning for non-R code chunks (thanks, @adamcagle, rstudio/rmarkdown#1528).

- When building package vignettes via `R CMD build`, the R scripts generated from **knitr**'s vignette (tangling) engines are not corrected encoded in UTF-8 (thanks, Kurt Hornik).

- Fixed a bug introduced when fixing #1648: `include_url()` no longer works at all unless the chunk option `out.extra` is not empty (thanks, @gabrielvazdemelo, #1691).

- When evaluating perl code chunks the interpreter is spawned with the `-E` option, enabling all optional features such as `say`, rather than `-e` (thanks, @kiwiroy, #1699)

- When the chunk label of a `tikz` chunk contains periods, the `tikz` output cannot be converted to the expected `.svg` (thanks, @ucpresearch, #1706).

- The `...` argument for `raw_latex()` and `raw_html()` is actually passed to `asis_output()` now (thanks, @GregorDeCillia, #1716).

## MAJOR CHANGES

- `knitr::knit()` will try to read the input file with the UTF-8 encoding first. If UTF-8 doesn't work, it will try to read with the system native encoding instead (with a warning). The `encoding` argument of `knitr::knit()` is completely ignored. In the future, only UTF-8 will be supported, and we will stop retrying with the system native encoding. The output file from `knitr::knit()` is always encoded in UTF-8.

- `spin_child()` will also assume the input file is encoded in UTF-8 (thanks, Henrik, https://stackoverflow.com/q/55395422/559676).

- When the chunk option `dev = 'svg'`, `grDevices::svg()` is used to record plots, instead of the default PDF null device (thanks, @trevorld, #729).

- Partially reverted the solution for #1403 in favor of fixing the more surprising issue #1708, i.e., avoid false positive cache dependencies among chunks more than false negative dependencies when the chunk option `autodep = TRUE` (thanks, @fountainer).

- The LaTeX package `framed.sty` is no longer bundled with the **knitr** package. This should only affect users who write `.Rnw` documents. If `framed.sty` is missing in your LaTeX distribution, you will have to install it by yourself. You may also consider TinyTeX (https://yihui.org/tinytex/), which has included this LaTeX package.

# CHANGES IN knitr VERSION 1.22

## NEW FEATURES

- Added chunk options `class.error`, `class.warning`, and `class.message` to customize the CSS classes for errors, warnings, and messages in R Markdown output, respectively (thanks, @gadenbuie, #1676).

- Added a new engine `sass`/`scss` to convert Sass/SCSS to CSS using either the **sass** [R package](https://github.com/rstudio/sass) (LibSass) or Dart Sass [executable](https://sass-lang.com/install) (when R package not found, the engine option `engine.opts = list(package = FALSE)`, or `engine.path` to executable is provided). After conversion, resulting CSS is treated as in the CSS engine (thanks, @emilyriederer, #1666).

- The `cat` engine supports the chunk option `eval = FALSE` now (thanks, @HanOostdijk, #1618).

- The chunk option `out.extra` can be used to include extra attributes for the `<iframe>` generated from `knitr::include_url()` or `knitr::include_app()` (thanks, @jvcasillas, #1648).

## BUG FIXES

- `knit_meta_add()` could exhaust the system's memory because of a wrong method of vectorization (thanks, @kevinushey @nikkoc, rstudio/rmarkdown#1538).

- The output path should be quoted in `pandoc()` (thanks, @antoine-sachet, #1644).

- When there are multiple figures with multiple captions in a code chunk, **bookdown**'s figure numbering is incorrect (thanks, Catherine Peng, https://stackoverflow.com/q/53880195/559676).

- Added an argument `label` to `kable()` so that users can manually specify a label (thanks, @Inferrator, #1655).

- The chunk option `fig.show='hide'` doesn't work for `knitr::include_graphics()` (thanks, @vincentarelbundock, #1675).

- The `tikz` engine doesn't work on Windows (thanks, Andry, https://stackoverflow.com/q/54839403/559676).

- `kable()` now generates a table for R Markdown documents even when the data has 0 rows (thanks, @yutannihilation, #1677).

- The chunk options `dev = 'tikz'` and `external = FALSE` didn't work in R Markdown (thanks, @martinschmelzer, #1649).

## MAJOR CHANGES

- `knitr::knit()` starts to warn against non-UTF8 encodings ([Why?](https://yihui.org/en/2018/11/biggest-regret-knitr/)). In the future, we will only support UTF-8 input. If your input file is not encoded in UTF-8, we strongly recommend that you re-save it with UTF-8.

- Removed the `encoding` arguments in `knitr::pandoc()`, `knitr::knit2pdf()`, `knitr2wp()`, and `knitr::Sweave2knitr()`. The input files must be encoded in UTF-8.

- `knitr::knit2html()` still has the `encoding` argument, but it only supports UTF-8 internally.

## MINOR CHANGES

- Changed `tools::texi2dvi()` to `tinytex::latexmk()` for the `tikz` engine to compile TikZ graphics to other formats (e.g., `svg` or `png`). This requires the **tinytex** package >= v0.10.10: https://github.com/yihui/tinytex.

- Empty lines are no longer removed in the output of `purl()` (thanks, Marius Hofert).

# CHANGES IN knitr VERSION 1.21

## NEW FEATURES

- Added **styler** support: the chunk option `tidy = 'styler'`, the code will be reformatted using the **styler** package. The chunk option `tidy = TRUE` means `tidy = 'formatR'` and still uses `formatR::tidy_source()` to reformat the code. The chunk option `tidy` can also take a custom function to reformat the code. See the documentation for details: https://yihui.org/knitr/options/ (thanks, @lorenzwalthert, #1581).

- Added a new hook function `hook_gifski()` to create a GIF animation from plots in a code chunk. To enable this hook, you may `install.packages('gifski')` and set the chunk option `animation.hook='gifski'`. See https://yihui.org/en/2018/08/gifski-knitr/ for examples.

- Added a new object `cache_engines` for other language engines to handle caching. See `?knitr::cache_engines` for details (thanks, @tmastny, #1518).

- Can now pass additional arguments to knitr vignette engines if needed (thanks, @jimhester, #1577).

- When `options(knitr.include_graphics.ext = TRUE)` is set, the full filename will be used in `\includegraphics{}` (e.g., `\includegraphics{foo.pdf}`) instead of the (default) base filename (e.g., `\includegraphics{foo}`) if a plot is embedded in the LaTeX output. This will avoid the ambiguity for LaTeX to choose the correct file (#1587).

- The chunk option `engine.opts` can also take a list now, so that users can specify different options for different language engines (thanks, @kiwiroy, #1632).

- Added functions `raw_block()`, `raw_latex()`, and `raw_html()` to wrap content in raw attribute blocks for Pandoc (thanks, @hughjonesd, #1633).

## BUG FIXES

- `valign` in `kable_latex()` does not put the float alignment to the correct location (thanks, @haozhu233, #1487, #1519).

- `engine.path` does not work for `engine = 'dot'` (thanks, @billy34, #1534).

- The `sql` engine now caches the result properly when the chunk option `output.var` is specified (thanks, @yutannihilation, #1544).

- `knit_params()` mangles UTF-8 text not representable in current locale (thanks, @kevinushey, #1557).

- The `linesep` argument in `kable(format = 'latex')` does not work as expected (thanks, @tinu-schneider, #1558).

- MP4 generated by `hook_ffmpeg()` does not work in Safari (thanks, @brianzhang01, #1586).

- The chunk option `fig.env` does not work for PDF output from R Markdown (thanks, @RSchwinn, #1620).

- `spin()` fails to generate a proper R Markdown document when the R script contains roxygen comments as string literals or more than three backticks (thanks, @yutannihilation, #1605, #1611).

- Stan chunks cannot be properly cached: the bug #1064 appeared again (thanks, Mark, https://stackoverflow.com/q/53660143/559676).

- Roman numerals should not be converted to arabic numerals in inline R expressions (thanks, @rgaiacs, #1625).

## MAJOR CHANGES

- For R scripts passed to `spin()`, only the UTF-8 encoding is accepted (previously it assumes the native encoding of the system).

- Due to changes in the S3 method dispatch mechanism since R 3.5.0, you will have to call `registerS3method()` to register `knit_print` methods _defined in code chunks_. For package authors, it no longer suffices to export a method without importing `knitr::knit_print`, but if you don't want to import it, there is still a workaround. See the package vignette `vignette('knit_print', package = 'knitr')` for details (thanks, @wch @kevinushey @skranz #1580).

## MINOR CHANGES

- `spin()` will close each individual code chunk if multiple code chunks appear consecutively (previously it only closes the last code chunk in this case) (thanks, @avehtari, #1590).

# CHANGES IN knitr VERSION 1.20

## NEW FEATURES

- Added a `roxygen_comments` parameter to `read_chunk()` in order to control whether trailing commentary is stripped from code chunks; default behaviour is to keep roxygen comments with `roxygen_comments = TRUE` (thanks, @ruaridhw, #1484).

## BUG FIXES

- `knit_filter()` does not work for R Markdown input files (thanks, @omahdi, #1498).

# CHANGES IN knitr VERSION 1.19

## NEW FEATURES

- `spin()` now supports code chunks of the form `-- ---- label ----` allowing external SQL and Haskell files to be spun (thanks, @ruaridhw, #1492).

## MAJOR CHANGES

- `tinytex::latexmk()` is used to compile LaTeX plots generated by the chunk option `dev = 'tikz'` to PDF, which means you have to install the **tinytex** package if you use this device.

- Similarly, `tinytex::latexmk()` is used in `knit2pdf()` and `stitch()`.

## BUG FIXES

- Fixed rstudio/bookdown#501: the chunk option `out.height` can also take a percentage value when the output format is LaTeX (similar to `out.width`), e.g., `out.height = '30%'` means `.3\textheight` (thanks, @dataopt, #1482).

- `imgur_upload()` starts to fail due to changes in Imgur's API (thanks, @jennybc, #1495).

# CHANGES IN knitr VERSION 1.18

## NEW FEATURES

- added a new engine `julia` based on `JuliaCall::eng_juliacall()` to support Julia code chunks (thanks, @Non-Contradiction, #1458)

- added a new chunk option `fig.dim` to specify `fig.width` and `fig.height` using one option, e.g., `fig.dim = c(5, 7)` is a shorthand of `fig.width = 5, fig.height = 7` (thanks, @MichaelChirico, #1449)

- added two chunk options `fig.ncol` and `fig.sep` to make it possible to arrange multiple subfigures in columns (thanks, @eliocamp, #1444)

- exported function `is_latex_output()` and `is_html_output()` (thanks, @zeehio, rstudio/rmarkdown#649)

- for `read_chunk()`, the code chunks in the external R script can be written under the lines of the form `-- ---- label ----` now (`# ---- label ----` is still supported); this makes it possible to read an external Haskell script (thanks, @brittAnderson, #1455)

- added a `$append()` method to the internal function `new_defaults()` (thanks, @yonicd, #1417)

- added another usage of `engine_output()` for expert users: if you are familiar with the list structure of the `evaluate::evaluate()` output, you can let your custom engine return a similar list and call `engine_output(options, out = YOUR_LIST)` (thanks, @kevinushey, #1442)

- added an `evaluate.inline` hook to `knit_hooks` (thanks, @kevinushey, #1443)

## MAJOR CHANGES

- the `python` engine is based on `reticulate::eng_python()` now; this means all Python code chunks are evaluated in the same Python session; if you want the old behaviour (new session for each Python code chunk), you can set the chunk option `python.reticulate = FALSE` (thanks, @kevinushey, #1440)

- the `auto_pdf` argument of `include_graphics()` defaults to `FALSE` now; you can set `options(knitr.graphics.auto_pdf = TRUE)` to change the default to `TRUE` (thanks, @mebden, #1456)

## BUG FIXES

- fixed #1446: **knitr** may fail to parse code chunks in blockquotes in Markdown (thanks, @rgaiacs)

- fixed #1460: `kable()` does not generate the table caption when the data only has one column (thanks, @Henrik-P)

- fixed #1464: subcaptions are not rendered if the chunk option `out.extra = NULL` and the output format is LaTeX (thanks, @eliocamp)

- fixed #1462: `kable()` may mis-order column names of data generated by **sparklyr** (thanks, @JohnMount)

# CHANGES IN knitr VERSION 1.17

## MAJOR CHANGES

- the automatic detection of chunk dependencies (via the chunk option `autodep = TRUE`) is more conservative by default now; chunk B depends on chunk A if _any_ variables in B are created in A, no matter if these variables are local or global in B; you can use the chunk option `cache.globals` to manually provide a vector of variable names that should be considered "global" to avoid the dependency when local variables in B are also found in A (thanks, @knokknok, #1403)

- for engines `css` and `js`, the source code will be displayed by default just like R code chunks, and you can hide the source code using the chunk option `echo = FALSE` (thanks, @skadauke, #1408)

- for `kable()`, the `digits` argument will be applied to columns of the class `difftime` (thanks, @RoyalTS, #1396)

## BUG FIXES

- when a table only has one column, `kable()` does not work in R Markdown documents (thanks, @expersso, https://github.com/yihui/printr/issues/31)

- `message(..., appendLF = FALSE)` does not work (thanks, Anastasius, http://stackoverflow.com/q/44059508/559676)

- `hook_optipng()` does not work correctly when a code chunk does not contain any R-generated plots (thanks, @@zevross-spatial, #1404)

- markdown lines starting with three backticks won't be considered inline code chunks (thanks, @gadenbuie, #1416)

# CHANGES IN knitr VERSION 1.16

## NEW FEATURES

- adds two new options, `class.output` and `class.source` to apply additional HTML classes to output and source chunks, which can be useful for R Markdown documents with HTML output formats since you can define custom CSS styles for these classes (thanks, @ijlyttle #1335 and @AmeliaMN #1333)

- added a `go` engine (thanks, @hodgesds, #1330)

- added a `sql.show_interpolated` chunk option for `sql` engine. For `{sql, sql.show_interpolated=TRUE}`, interpolated SQL query will be displayed in the rendered document instead (thanks, @saurfang, #1357)

- the chunk option `results=FALSE` is an alias of `results='hide'` now (thanks, @hadley, #1360)

- added a wrapper function `knit2pandoc()`, and reStructuredText vignettes will be compiled through Pandoc instead of rst2pdf (thanks, @trevorld, #1386)

- longest common whitespace prefix in code chunks is now stripped off along with the eventual prefix (see #1391)

## MAJOR CHANGES

- a warning will be emitted when the chunk option `fig.show='hold'` and the output format is Word (https://github.com/rstudio/bookdown/issues/249); `fig.show='hold'` will be changed to `'asis'` at the same time

- `purl()` no longer writes verbatim non-R code chunks to the output R script; these code chunks will be commented out in the output

- `purl()` can now also retrieve the code from inline expressions; it is disabled by default for compatibility, and can be enabled via `options(knitr.purl.inline = TRUE)` (thanks, @kforner, #1345)

- the chunk option `fig.show` also applies to plots included via `include_graphics()`, e.g. `fig.show='hold'` will put all plots to the end of a chunk (thanks, @statnmap, #1382)

## BUG FIXES

- `kable()` should not ignore the table caption if provided when the input is a list, the table format is `pandoc`, and the document output format is not HTML or LaTeX, e.g. Word output (thanks, @ugroempi, https://github.com/rstudio/bookdown/issues/368)

- `opts_chunk$set()` supports referencing a `sql` connection by variable name, e.g. `knitr::opts_chunk$set(connection = "db")` (thanks, @javierluraschi, #1362)

- the chunk option `dev = 'CairoPS'` will lead to double filename extensions `.eps.ps` (thanks, @qifei9, #1364)

- the output file of `write_bib()` is now encoded in UTF-8 (thanks, @arnyeinstein, #1374)

- the `<embed>` tag should be used instead of `<img>` for PDF images in HTML output (thanks, Daijiang Li, http://stackoverflow.com/q/43549930/559676)

# CHANGES IN knitr VERSION 1.15.1

## NEW FEATURES

- added a new hook function `hook_pngquant()` that can call `pngquant` to optimize PNG images (thanks, @slowkow, #1320)

## BUG FIXES

- not really a **knitr** bug, but `knit_params()` should be better at dealing with multibyte characters now due to the bug fix in the **yaml** package https://github.com/viking/r-yaml/issues/6

# CHANGES IN knitr VERSION 1.15

## NEW FEATURES

- `NA` values can be displayed using different characters (including empty strings) in `kable()`; you can set the option `knitr.kable.NA`, e.g. `options(knitr.kable.NA = '')` to hide `NA` values (#1283)

- added a `fortran95` engine (thanks, @stefanedwards, #1282)

- added a `block2` engine for R Markdown documents as an alternative to the `block` engine; it should be faster and supports arbitrary Pandoc's Markdown syntax, but it is essentially a hack; note when the output format is LaTeX/PDF, you have to define `\let\BeginKnitrBlock\begin \let\EndKnitrBlock\end` in the LaTeX preamble

- figure captions specified in the chunk option `fig.cap` are also applied to HTML widgets (thanks, @byzheng, https://github.com/rstudio/bookdown/issues/118)

- when the chunk option `fig.show = 'animate'` and `ffmpeg.format = 'gif'`, a GIF animation of the plots in the chunk will be generated for HTML output (https://twitter.com/thomasp85/status/785800003436421120)

- added a `width` argument to `write_bib()` so long lines in bib entries can be wrapped

- the inline syntax `` `r#code` `` is also supported besides `` `r code` ``; this can make sure the inline expression is not split when the line is wrapped (thanks, Dave Jarvis)

- provided a global R option `knitr.use.cwd` so users can choose to evaluate the R code chunks in the current working directory after setting `options(knitr.use.cwd = TRUE)`; the default is to evaluate code in the directory of the input document, unless the **knitr** option `opts_knit$set(root.dir = ...)` has been set

- if `options(knitr.digits.signif = TRUE)`, numbers from inline expressions will be formatted using `getOption('digits')` as the number of significant digits, otherwise (the default behavior) `getOption('digits')` is treated as the number of decimal places (thanks, @numatt, #1053)

- the chunk option `engine.path` can also be a list of paths to the engine executables now, e.g., you can set `knitr::opts_chunk$set(engine.path = list(python = '/anaconda/bin/python', perl = '/usr/local/bin/perl'))`, then when a `python` code chunk is executed, `/anaconda/bin/python` will be called instead of the system default (rstudio/rmarkdown#812)

- introduced a mechanism to protect text output in the sense that it will not be touched by Pandoc during the conversion from R Markdown to another format; this is primarily for package developers to extend R Markdown; see `?raw_output` for details (which also shows new functions `extract_raw_output()` and `restore_raw_output()`)

## MAJOR CHANGES

- the minimal version of R required for **knitr** is 3.1.0 now (#1269)

- the **formatR** package is an optional package since the default chunk option `tidy = FALSE` has been there for a long time; if you use `tidy = TRUE`, you need to install **formatR** separately if it is not installed

- `:set +m` is no longer automatically added to `haskell` code chunks (#1274)

## MINOR CHANGES

- the package option `opts_knit$get('stop_on_error')` has been removed

- the confusing warning message about `knitr::knit2html()` when building package vignettes using the `knitr::rmarkdown` engine without `pandoc`/`pandoc-citeproc` has been removed (#1286)

- the default value of the `quiet` argument of `plot_crop()` was changed from `!opts_knit$get('progress')` to `TRUE`, i.e., by default the messages from cropping images are suppressed

## BUG FIXES

- the chunk option `cache.vars` did not really behave like what was documented (thanks, @simonkth, #1280)

- `asis_output()` should not be merged with normal character output when `results='hold'` (thanks, @kevinushey, #1310)

# CHANGES IN knitr VERSION 1.14

## NEW FEATURES

- improved caching for Rcpp code chunks: the shared library built from the C++ code will be preserved on disk and reloaded the next time if caching is enabled (chunk option `cache = TRUE`), so that the exported R functions are still usable in later R code chunks; note this feature requires Rcpp >= 0.12.5.6 (thanks, @jjallaire, #1239)

- added a helper function `all_rcpp_labels()`, which is simply `all_labels(engine == 'Rcpp')` and can be used to extract all chunk labels of Rcpp chunks

- added a new engine named `sql` that uses the **DBI** package to execute SQL queries, and optionally assign the result to a variable in the **knitr** session; see https://rmarkdown.rstudio.com/authoring_knitr_engines.html for details (#1241)

- `fig.keep` now accepts numeric values to index low-level plots to keep (#1265)

## BUG FIXES

- fixed #1211: `pandoc('foo.md')` generates foo_utf8.html instead of foo.html by default

- fixed #1236: `include = FALSE` for code chunks inside blockquotes did not work (should return `>` instead of a blank line) (thanks, @fmichonneau)

- fixed #1217: define the command `\hlipl` for syntax highlighting for Rnw documents (thanks, @conjugateprior)

- fixed #1215: restoring `par()` settings might fail when the plot window is partitioned, e.g. `par(mfrow = c(1, 2))` (thanks, @jrwishart @jmichaelgilbert)

- fixed #1250: in the quiet mode, `knit()` should not emit the message "processing file ..." when processing child documents (thanks, @KZARCA)

## MAJOR CHANGES

- **knitr** will no longer generate screenshots automatically for HTML widgets if the **webshot** package or PhantomJS is not installed

## MINOR CHANGES

- if `dev = 'cairo_pdf'`, the `cairo_pdf` device will be used to record plots (previously the `pdf` device was used) (#1235)

- LaTeX short captions now go up to the first `.`, `:` or `;` character followed by a space or newline (thanks, @knokknok, #1249)

# CHANGES IN knitr VERSION 1.13

## NEW FEATURES

- `kable` now accepts a single multi-character string for alignment, so `kable(..., align=c('c','l'))` can be shortened to `kable(..., align='cl')`

- code chunks that generate metadata may be cached now; it requires htmlwidgets >= v0.6 and htmltools >= 0.3.3 if you cache code chunks that contain HTML widgets or Shiny inputs/outputs (#1158)

- when the output format is not HTML, HTML widgets used to fail to render; now **knitr** will try to generate static screenshots for HTML widgets automatically using the [**webshot**](https://github.com/wch/webshot) package; you can specify alternative screenshots via the chunk option `screenshot.alt` (which takes a character vector of image paths), and pass more options to `webshot::webshot()` via the chunk option `screenshot.opts`, e.g. `list(delay = 3, cliprect = 'viewport')`

- added two functions `include_url()` and `include_app()` to embed URLs in the output (the latter is for Shiny app URLs); when the output format is HTML, iframe will be used; otherwise screenshots of the URLs will be used

- screenshotting for HTML widgets and URLs can be forced for all output formats via the chunk option `screenshot.force = TRUE`; if you set the chunk option `screenshot.force = FALSE`, **knitr** will just render these content normally and not take screenshots

- added a new chunk option `fig.link`, which can be used to attach hyperlinks on figures, e.g. you can add a link to a screenshot of a Shiny app so that readers can check out the actual live app after clicking on the static screenshot (this chunk option currently only works for Markdown and LaTeX output)

- syntactical errors in code chunks will be allowed when the chunk option `error = TRUE` and the package version of **evaluate** is at least 0.8.4; previously **knitr** would just stop on parsing errors (https://github.com/hadley/evaluate/issues/65)

- PNG/JPEG images included via `include_graphics()` also respects the chunk option `dpi` now; if it is numeric and the chunk option `out.width` is not set, the output width (in inches) of an image will be automatically  calculated from the actual width (in pixels) divided by `dpi`; note this feature requires the packages **png** and/or **jpeg** to be installed, and you can disable the feature using `dpi = NA` (thanks, @hadley, https://github.com/rstudio/bookdown/issues/38)

- added a new hook function named `evaluate` in `knit_hooks` so that users can redefine an evaluator to evaluate the code chunk; the default is `evaluate::evaluate()`, and your custom evaluator must be compatible with `evaluate::evaluate()` in terms of the argument names and the data structure of the returned value (a list of values with special classes)

- added a new function `combine_words()` to combine multiple words / phrases into a single string, which may be useful in inline R expressions, e.g. `combine_words(c('a', 'b', 'c'))` returns `a, b, and c`

- `render_markdown()` gained a new argument `fence_char` to customize the character to be used as the code blocks fence, e.g. it can be a backtick, or a tilde, depending on the Markdown rendering engine (thanks, @tinyheero, #1161)

- the `pandoc()` function no longer assumes Markdown input (thanks, @scls19fr, #1170)

- added a new function `knit_meta_add()` so that users can manually inject metadata into the current **knitr** session

- for the `tikz` engine, if `fig.ext = 'svg'`, `dvisvgm` will be called to convert the DVI output of TikZ to SVG; you need to install `dvisvgm`, and Windows users have to install GhostScript as well (thanks, @dkilfoyle, #1177)

- new `js` and `css` engines which surround their content with `<script>` and `<style>` tags respectively, and print no output when not in an HTML document

- for LaTeX tables, `kable()` supports short captions now via the `caption.short` argument, e.g. `kable(..., caption = 'A long caption', caption.short = 'A short caption')` (thanks, @ismayc, #1199)

- added three global R options `knitr.sanitize.errors`, `knitr.sanitize.warnings`, and `knitr.sanitize.messages` to mask or change the messages, e.g. if `options(knitr.sanitize.errors = TRUE)` and the chunk option `error = TRUE`, the actual error message will be replaced by a character string like `"An error occurred"`; these options can also accept character values so you can customize the messages to be displayed, e.g. `options(knitr.sanitize.warnings = 'You had a warning from the code')`; see https://github.com/rstudio/shiny/issues/1123 for the motivation of these options

## BUG FIXES

- when the chunk option `cache.rebuild = TRUE`, the cache database should be rewritten (thanks, Oleg Mayba)

- `include_graphics()` did not work in inline R expressions (thanks, @WastlM, #1166)

- the `cex` parameter was not correctly restored in the case of `opts_knit$set(global.par = TRUE)` (http://stackoverflow.com/q/35606445/559676)

- for Rnw documents, when there are two instances of `\documentclass{}`, **knitr** might mistakenly treat the second instance as the actual command to declare the document class (thanks, #1180, @ekstroem)

- corrected the environment for evaluating R scripts in `stitch_rhtml()` and `stitch_rmd()` (thanks, @Hughan, #1207)

## MAJOR CHANGES

- the default value of the package option `eval.after` is changed from `NULL` to `fig.cap`, i.e. the figure caption will always be evaluated after a code chunk is evaluated (thanks, @JoshOBrien, #1165)

- the function `eclipse_theme()` has been removed since the website eclipsecolorthemes.org has been down for a long time

# CHANGES IN knitr VERSION 1.12.3

## BUG FIXES

- figures without captions may fail to render correctly in R Markdown v2

- fixed #1149: the `sanitize` and `external` chunk options were not passed to `tikzDevice::tikz()` when the `tikz()` device was opened to record plots in a chunk, so plots that contain special LaTeX characters would not work (thanks, @O-T)

# CHANGES IN knitr VERSION 1.12

## NEW FEATURES

- added a chunk option `fig.asp` to specify the aspect ratio (i.e. the ratio height/width) of figures: when `fig.asp` is specified, the height of a figure (the chunk option `fig.height`) is calculated from `fig.width * fig.asp`

- added a new function `knit_watch()` to knit input files continuously when any of them are updated, so you do not have to call a knitting function like `knit()`, `knit2pdf()`, or `rmarkdown::render()` repeatedly as you update an input document

- `kable()` supports generating multiple tables from a list of data objects, and the tables will be placed side by side when the output format is HTML or LaTeX

- the `format` argument of `kable()` can be a function that returns a character string of the table format (thanks, @haozhu233, #1132)

- added a new function `include_graphics()` to embed existing image files into **knitr** documents; see `?include_graphics` for details

- added a new object `opts_hooks` that can be used to set up chunk option hooks to modify chunk options; see `?opts_hooks` for details (thanks, @hadley, #1096)

- added a chunk option `ffmpeg.bitrate` (default: `1M`) to control the quality of WebM videos created from FFmpeg (thanks, @JsonPunyon, #1090)

- added a chunk option `ffmpeg.format` (default: `webm`) to control the video format of FFmpeg; this option specifies the filename extension of the video (#712)

- plots drawn by `tikzDevice` are also measured and recorded with that device (thanks, @krlmlr, #1066)

- for the chunk option `out.width`, a value of the form `xx%` will be converted to `xx/100\\linewidth` when the output format is LaTeX, so we can specify figure widths as percentages for both LaTeX and HTML output

- for the chunk option `background` (to define the background color of code chunks in Rnw documents), the value `NA` means **knitr** will no longer write `\definecolor{shadecolor}` to the output of an individual code chunk (you can also enable this behavior by setting it as a global chunk option) (thanks, @friendly, #1122)

- added a new engine `block` to wrap the chunk in a LaTeX environment or an HTML `div`, depending on the output format; the name of the environment or the class of the `div` can be specified via the chunk option `type`

- you can use the `svglite` device in the **svglite** package (https://github.com/hadley/svglite) via the chunk option `dev = 'svglite'`

- `cache = TRUE` works better for the `stan` engine now: the Stan model object will be saved and can be loaded the next time for the cached code chunk (thanks, @dalewsteele, #1064)

- the chunk option `engine.env` can be used to set environment variables when running engines like `bash`; this option is passed to the `env` argument of `system2()` (thanks, @mbojan, #1032)

## BUG FIXES

- the `asis` engine did not preserve line breaks in the code chunk

- `kable()` could not display chron objects correctly (thanks, @liesb, #1118)

- `kable(format = 'latex', longtable = TRUE)` did not write the table caption in the correct place (should be inside the `longtable` environment)

- fixed #1055: the `tikz` device may fail if there are code chunks before `\documentclass{}` in Rnw documents (thanks, @ClaudiusL)

- fixed #1119: when the chunk option `engine != 'R'`, the `size` option does not work (thanks, @cysouw)

## MAJOR CHANGES

- local chunk options can override options set in `opts_template` now, e.g. for `opts_template$set(quiet = list(echo = FALSE))`, and a local chunk option `echo = TRUE` will override `echo = FALSE` in `opts_template` for the current code chunk (thanks, @aaronwolen, #1140)

## MINOR CHANGES

- added an argument `force_v1` to `knit2html()`; when it is `TRUE`, `knit2html()` renders the input file as an R Markdown v1 document even if it is for R Markdown v2 (#1130)

- the trailing space after `![]()` (for plots) in Markdown output has been removed (#1049)

# CHANGES IN knitr VERSION 1.11

## NEW FEATURES

- added an argument `format.args` to `kable()` to pass arguments to `format()` mainly to format numeric values, e.g. you can use `,` as the thousands separator via `kable(..., format.args = list(big.mark = ','))` (thanks, @neuwirthe, #1086)

- added an argument `quiet` to `plot_crop()` (thanks, @WastlM, #1034)

- added `!expr` and `!r` support in YAML for `knit_params()` (#1069)

- added two language engines `mysql` and `psql` (thanks, @beanumber, #1076)

## MAJOR CHANGES

- the two hook functions `hook_rgl()` and `hook_webgl()` have been moved from **knitr** to the **rgl** package (v0.95.1247) (thanks, @dmurdoch)

- inline R expressions will always be evaluated; previously, they were not be evaluated if the global chunk option `eval = FALSE`, and the inline output was `??` (thanks, @Bozh, #1046)

## BUG FIXES

- the HTML code generated for FFmpeg animations did not work through Pandoc conversion (thanks, @s-fiebig, #1067)

# CHANGES IN knitr VERSION 1.10.5

## MAJOR CHANGES

- this version is a patch release due to a few changes in handling the `params` field of the YAML metadata, including
    - the character input passed to `yaml::yaml.load()` will always be converted to UTF-8 to make sure multibyte characters work in YAML
    - the character output from `yaml::yaml.load()` will always be marked as UTF-8 (https://github.com/viking/r-yaml/issues/6)
    - `y`, `Y`, `n`, and `N` will not be treated as booleans (`TRUE`/`FALSE`), although they do stand for booleans in the YAML 1.1 spec
    - for `purl()`, the `params` field will be correctly written to the R script

# CHANGES IN knitr VERSION 1.10

## NEW FEATURES

- added a new chunk option `cache.rebuild` (default: `FALSE`) to force **knitr** to invalidate and rebuild the cache for a chunk when `cache.rebuild = TRUE` (thanks, @zachary-foster, #995)

- added a `stata` engine and improved the existing `sas` engine (thanks, @Hemken #979, @muschellij2 #984)

- the Markdown table generated from `kable()` will use the width of characters to decide how many spaces are needed for padding, which can be useful for wide characters such as CJK characters (normally of width 2 per char) (thanks, @yutannihilation, #985)

- for language engines, the syntax of R Markdown code chunk headers can be ```` ```{lang, option=value} ```` now (e.g. ```` ```{python} ```` and ```` ```{Rcpp} ````), which is equivalent to ```` ```{r, engine='lang', option=value} ```` (thanks, @JanSchulz, #963)

- added the `...` argument to `all_labels()` to make it possible to filter chunk labels according to local chunk options, e.g. you can use `all_labels(engine == 'Rcpp')` to find the labels of all code chunks that have the chunk option `engine = 'Rcpp'`, and pass the character vector of labels to `ref.label` to gather all `Rcpp` code chunks into a single chunk

- added a new function `knit_params()` to extract report parameters from the YAML metadata of R Markdown documents; see `?knit_params` for details (thanks, @jjallaire, #997)

- added support for code chunks inside Markdown blockquotes; see the example below (thanks, @r-gaia-cs, #1022)

        > Here is a quote, followed by a code chunk:
        >
        > ```{r}
        > x = 1:10
        > rev(x^2)
        > ```

- the function `current_input()` gained a new argument `dir = FALSE`; when `dir = TRUE`, the absolute path of the input document is returned (thanks, @klmr, #950)

- if you set `options(knitr.duplicate.label = 'allow')` before calling `knit()`, duplicate labels will be allowed, and an incremental suffix will be appended to the duplicate label, e.g. when there are two non-empty chunks with the same label `foo`, the second label may be converted to `foo-2` automatically; please use this feature only if you understand the consequences (e.g. the plot generated by a chunk `foo` might be `foo-2-1.pdf`, which can be surprising) (thanks, @khughitt, #957)

- added a new language engine named `lein` to support Clojure (thanks, @kindlychung, #940)

## MAJOR CHANGES

- the minimal required version of R is 3.0.2 now, and you are recommended to use the latest version of R when possible

## BUG FIXES

- fixed #942: the package option `opts_knit$get('base.dir')` does not work when the chunk option `opts_chunk$get('fig.path')` contains a directory (thanks, @daattali and @cheebingloh)

- fixed the bug reported at http://stackoverflow.com/q/28954647/559676 (`dev = 'png'` failed to work with **rmarkdown** when the output format is `pdf_document`)

# CHANGES IN knitr VERSION 1.9

## NEW FEATURES

- added a new function `load_cache()` so that we can read an object from a cached code chunk, even if the chunk appears later in the document and the object needs to be used earlier; see the example #114 at https://github.com/yihui/knitr-examples (related to #868)

- added a new function `clean_cache()` to clean up the cache files that are probably no longer needed, e.g. after you rename or delete cached code chunks (thanks, @sjmgarnier, #933)

- `knit2wp()` can update an existing post or create a page now (thanks, @jaredlander, #916, #917)

- added an engine `stan` to support [Stan](http://mc-stan.org) through the **rstan** package; see http://rpubs.com/jrnold/knitr-stan for an example (thanks, @jrnold, #903)

- for the `tikz` engine, the path to the `convert` utility of ImageMagick can be specified via the chunk option `engine.opts`, e.g. `engine.opts = list(convert = 'path/to/convert')` (thanks, @mienkoja, #897)

- similarly, more command line arguments to `convert` can be specified via, for example, `engine.opts = list(convert.opts = '-density 300')` (#896)

## MAJOR CHANGES

- when the chunk option `dev = 'png'`, `grDevices::png()` is used to record plots, instead of the default PDF null device (thanks, @yixuan, #729)

- currently, R (3.1.2) does not really pass the vignette encoding to vignette engines (which is probably a bug), and **knitr** vignette engines will assume UTF-8 is the file encoding

- when the chunk option `tidy=FALSE`, and `eval` takes a numeric vector, it used to mean the line numbers of the code chunk; now it means the indices of the R expressions in the code chunk, regardless of `tidy=FALSE` or `TRUE` (yihui/knitr-examples#39, thanks, @isomorphisms)

## MINOR CHANGES

- you need to upgrade the **rgl** package to at least v0.95.1201 if you use the hook function `hook_webgl()` (thanks, @dmurdoch, #901)

- for the automatically generated figure caption and label of a figure, the label is placed _outside_ of the caption now, i.e. the format was changed from `\caption{\label{}}` to `\caption{}\label{}` (thanks, @dr-moebius, @krlmlr, @ltorgo, #746)

## BUG FIXES

- fixed #898: `kable()` did not work on matrices with duplicate row names (thanks, @tomaskrehlik)

- fixed #927: `kable()` did not work for matrices of zero row and/or zero column (thanks, @hadley)

- fixed #929: `opts_knit$restore()` does not restore the `animation.fun` option (thanks, @julian-gehring)

# CHANGES IN knitr VERSION 1.8

## NEW FEATURES

- when using **knitr** with the **rmarkdown** package, the internal output hook for plots will be automatically switched to the LaTeX plot hook when necessary; for example, when the chunk options fig.align, out.width, out.height, and/or out.extra are specified, raw LaTeX code will be generated to align/set the size of plots, because there is no support for figure alignment or setting the size in the native Markdown syntax; for Word output, these options are simply ignored (related issues: #626, rstudio/rmarkdown#86, rstudio/rmarkdown#148, rstudio/rmarkdown#303)

- added a new function `fig_chunk()` to provide a public API to get the figure filenames produced from code chunks; since **knitr** 1.7 changed the figure file numbering scheme, it broke documents with hard-coded figure filenames, e.g. for Rnw documents, `\includegraphics{foo.pdf}` should be `\includegraphics{foo-1.pdf}` after **knitr** 1.7, and such problems can be avoided by `\includegraphics{\Sexpr{fig_chunk('foo', 'pdf')}}` (thanks, @edwardabraham, #870)

- added an argument `escape = TRUE` to `kable()` to escape special characters in LaTeX and HTML tables (thanks, @juba, #852)

- added a new function `knit_filter()` to filter out code chunks and inline R expressions; this function can be used as the filter for the spell check function `utils::aspell()`; see `?knit_filter` for examples (#581)

- added a new function `spin_child()` to spin child R scripts when we `spin()` a main script (thanks, @krlmlr, #621)

- added a new function `inline_expr()` to help authors write the "source code" of the inline expression, e.g. `inline_expr('1+1')` generates `` `r 1+1` `` in R Markdown documents (#890)

- the cache will attempt to preserve the order in which packages are stored on the search path (thanks, @dgrtwo, #867)

- added a new argument `table.envir` to `kable()` for LaTeX tables only; if the table caption is specified (not `NULL`), the LaTeX environment `table` will be used by default (i.e. the table is generated in `\begin{table} \end{table}`), and you can specify alternative environments via `kable(..., table.envir = '???')` (thanks, @dalupus, #872)

- chunk options are supported using the syntax `# ---- label, options ----` in the R script passed to `stitch()` (thanks, @wibeasley, yihui/knitr-examples#35)

- syntax highlighting for .Rnw and .Rhtml documents can be further customized by setting `opts_knit$set(highr.opts = list(markup = cmd_mine))` where `cmd_mine` is a data frame for the `markup` argument of `highr::hilight()` (thanks, @lq, #869)

- added a new language engine `groovy` (thanks, @vveitas, #876)

## BUG FIXES

- fixed #862: the YAML metadata in child R Markdown documents was not correctly removed (thanks, @krlmlr)

- fixed #874: for the engines `dot` and `tikz`, the figure directory will be created recursively (thanks, @WilDoane)

- fixed #654: sub figures were not aligned correctly in LaTeX when the chunk option `fig.align` was specified (thanks, @lionandoil)

- the vignette engine `knitr::rmarkdown_notangle` did not really work (thanks, @bbolker, http://stackoverflow.com/q/26726388/559676)

# CHANGES IN knitr VERSION 1.7

## NEW FEATURES

- added a simple Fortran engine after sitting with John Nash for a few minutes at UseR!2014; now we can use the chunk option `engine = 'fortran'` to include Fortran code in a source document, which will be compiled and loaded via `R CMD SHILIB` and `dyn.load()`, respectively

- added vignette engines with the suffix `_notangle`, which have the same weave functions as those engines without this suffix but have disabled the tangle function, meaning there will not be R scripts generated from the vignettes during `R CMD build` or `R CMD check` (thanks, Carl Boettiger and Michael Koohafkan, #784)

- added an argument `col.names` to `kable()`, so we can specify different column names (thanks, @jackflibb, #801)

- added a new output hook called `text`, and its default value `knit_hooks$get('text')` is the identity function `function(x) x`; this hook is applied to the text chunks (recall `knit_hooks$get('chunk')` is applied to code chunks)

- added a chunk option `fig.showtext` to support the **showtext** package; if `fig.showtext = TRUE` (which is what you should do if you use the **showtext** package), `showtext::showtext.begin()` is called before drawing plots (thanks, @yufree, #799)

- added a new language engine `node` for Node.js (thanks, Jake Burkhead, #823)

- added a package option `global.par`; if we set `opts_knit$set(global.par = TRUE)` (by default it is FALSE), the `par()` settings from the last code chunk will be preserved and applied to the next code chunk (thanks, Jim Winget)

- language engines also write error messages (if there are any) in the output now (thanks, Fabian Hirschmann, #789)

- added the `envir` argument to `knit_child()` so that users can specify a different environment to evaluate the child documents (thanks, Stéphane Laurent, http://stackoverflow.com/q/24009622/559676)

- for `set_parent()`, the lines in the parent document that start with `\bibliography` are matched and inserted in the child document so LaTeX bibliography also works for the child document (thanks, Mark Heckmann, #819)

- for the chunk option `engine = 'cat'`, the code chunk can be displayed in the output if the chunk language is specified via `engine.opts`, e.g. `engine.opts = list(lang = 'makefile')`

## BUG FIXES

- fixed #779: when the chunk options `tidy=FALSE` and `eval=FALSE`, `prompt=TRUE` did not work for R expressions of multiple lines (thanks, Qijie Zhao)

- fixed #788: there was no increment in the chunk counter when the code chunks were read through `read_chunk()`, which may lead to clashes of chunk labels (thanks, Jason Ackman)

- fixed #790: when chunk A reuses code from chunk B via `<<B>>`, and only the first line of B is empty, chunk reuse can fail because A sees B as empty (thanks, @kingaa)

- fixed #791: if one has specified the chunk option `dev.args`, only `pointsize` and `bg` in it can be passed to the default recording device (the `pdf()` device) (thanks, @M-Russell and @joelgombin)

- fixed #822: `cache.lazy = FALSE` did not really work (thanks, Liz Ing-Simmons)

- fixed rstudio/rmarkdown#205: when R marks the encoding of the input document as latin1, `knit()` can fail to convert its encoding (thanks, @ripkrizbi)

- fixed #828: scientific notation for inline numbers did not work in R Markdown v2 when the output format is LaTeX (thanks, @nacnudus)

- fixed #833: for the LaTeX output format, when `fig.cap` contains `.` in `{}`, the automatic short caption does not work (thanks, Roman Luštrik)

- fixed #844: when the `digits` argument is a vector and `x` is a numeric matrix, `kable(x)` did not work (thanks, @dmenne, #844)

## MAJOR CHANGES

- the `knit()` function no longer modifies R's default `options(digits)` from 7 to 4, since it may lead to confusion especially when printing `summary()` output; for those who want the old behavior, you must set `options(digits = 4)` at the beginning of your document (thanks, John Honaker, #777)

- the figure file numbering scheme has changed: for a chunk with a label `foo`, its figure files are named as `foo-i` where `i` ranges from `1` to `n` (the total number of plots in this chunk); previously, the figure file was named as `foo` instead of `foo-1` when there was only one plot generated in this chunk, which has a potential bug: consider two chunks named `foo` and `foo2`, respectively; `foo` generates two figures `foo1.png` and `foo2.png`, and `foo2` generates one figure `foo2.png`, which will overwrite the second figure generated from the chunk `foo` (thanks, @kevinushey, @kohske, @kforner, #704, #832)

- for warnings and errors from code chunks, the call that produced them will be printed as part of the message, e.g. previously an error might just be `Error: x must be positive`, and now it may be `Error in FUN(x = -1): x must be positive` (thanks, @jennybc, #846)

- for the engine `coffee` (CoffeeScript), the flag `-p` has been removed from the command line arguments, which means the default behaviour of this engine is to evaluate the code, instead of printing JavaScript; if you want the old behaviour, you need the chunk option `engine.opts = '-p'` (thanks, Jake Burkhead, #821)

- when the chunk option `results = 'hold'`, the text output blocks will be collapsed into a single block (thanks, Gavin Simpson, #798)

- the video format for animations (when the chunk option `fig.show='animate'`) was changed from OGG to WebM (http://www.webmproject.org), which has many benefits over other formats, especially for the web (thanks, @gaorongchao, #641)

- the YAML metadata in Markdown child documents will be ignored (only the metadata in the top parent document is preserved)

## MINOR CHANGES

- scientific formatting for inline R output is only applied to objects of which the first class is `numeric`, e.g. `chron::chron()` objects will no longer be formatted using scientific notations (thanks, @sanfordweisberg, #806)

- for R Markdown v2 documents, if the inline R output is formatted using the scientific notation, the output must be put in a math environment, e.g. `` $`r 2e10`$ ``

# CHANGES IN knitr VERSION 1.6

## NEW FEATURES

- added Textile (http://txstyle.org) support thanks to Richard Cotton; see the example 105 at https://github.com/yihui/knitr-examples (#623)

- added an argument `inline` to `spin()` so that some R code (by default, code of the form `{{code}}`) can be treated as inline expressions (thanks, Kirill Müller, #620)

- added an argument `prefix` to `write_bib()` so that we can customize the prefix for bib entries; we can also set `options(knitr.bib.prefix = 'a_string')` so that `write_bib()` uses this global option as the default value for `prefix` (thanks, Michael Friendly)

- the chunk option `dev.args` will be passed to the `pdf` recording device if `pdf` is also in the chunk option `dev` (note the recording device can be different with the actual drawing device); for example, you can pass a different `pointsize` to the PDF recording device (thanks, Felix Klein)

- intermediate files generated during `spin()` are deleted by default, this can be overridden by setting the new parameter `precious` to `TRUE` (thanks, Kirill Müller, #628)

- exposed the previously internal chunk option `code`, which is used to store the source code of a chunk; now users also have control over this option, which means we can programmatically assign source code for a code chunk, e.g. `code = capture.output(dump('fivenum', ''))`

- added a new chunk option `collapse` for Markdown output; if `collapse = TRUE`, **knitr** will try to merge the source and output blocks into one block; see example 039 at https://github.com/yihui/knitr-examples (thanks, Hadley Wickham)

- added a new chunk option `fig.retina` for better display quality of images in HTML output; for example, the physical size of an image is doubled and its display size is halved when `fig.retina = 2`

- added a new chunk option `strip.white` to trim the white lines in the beginning or end of a source chunk in the output (`strip.white = TRUE` by default)

- added a new chunk option `render` for custom rendering of R objects in the output; by default, the render function is `print()` (or `show()` for S4 objects) (thanks, Milan Bouchet-Valat, #484)

- added a new S3 generic function `knit_print()`, which is used as the default printing function for R objects in the code output now; users can define custom S3 methods to change the printing behaviour of R objects in **knitr**

- added a new engine `scala` for Scala thanks to Simeon Fitch (#640)

- added a new engine named `asis` to write the chunk content without processing it; it also respects the chunk options `echo` and `eval` -- when either one is `FALSE`, the chunk will be hidden; this makes it possible to write text conditionally (thanks, Simon, #622)

- the Haskell engine supports multiline clauses using `:set +m` now, thanks to Adam Vogt (#633)

- added a new hook function `hook_purl()` so that code can be more reliably extracted from input documents; see `?hook_purl` for details (#607)

- exported a function `plot_crop()` which was used in `hook_pdfcrop()` in previous versions to crop the white margin of plots

- added a new chunk option `cache.lazy` (TRUE/FALSE) to decide whether to save and lazy load objects when `cache` is enabled; for really large objects, you may need `cache.lazy=FALSE` (thanks, Dario Strbenac, Scott Simpkins, and Mattrition, #572)

- added a chunk option `cache.comments` (TRUE/FALSE) to decide whether updating R comments in a code chunk should invalidate the cache database (thanks, @knokknok, #718)

- `kable()` supports alignment for HTML tables now (thanks, Joseph Larmarange, #656 and #683)

- `kable()` supports table captions in LaTeX, HTML and Pandoc's Markdown now (thanks, Joseph Larmarange, #687)

- the `digits` argument for `kable(x)` can also be a vector of length `ncol(x)`, in which case the number of digits is set for each column separately, e.g. `kable(data.frame(x=rnorm(10), y=rnorm(10)), digits = c(1, 4))` (thanks, @nacnudus, #771)

- for Markdown tables, `kable()` gained a new argument `padding` to specify the inner padding of table cells using spaces (thanks, @gavril0, #699)

- added a new vignette engine called `rmarkdown`, which uses `rmarkdown::render()` to create a package vignette from an R Markdown document; see https://rmarkdown.rstudio.com for more information about the **rmarkdown** package, and the vignette `knit_print.Rmd` in **knitr** for an example (basically you specify `\VignetteEngine{knitr::rmarkdown}` in your vignette)

- indentation is preserved when using chunk references `<<>>`, i.e., if `<<>>` is indented, the spaces before it will be applied to the code that it refers to (thanks, Terry Therneau)

- added a chunk option `fig.process`, which can be set as a function to take the path of a plot and process it, e.g. `fig.process = function(x) knitr::plot_crop(x)`; note that the `fig.process` function must return a character string, such as the path of the figure, which might have been changed inside the function

- added a chunk option `R.options` so that we can temporarily set local options() for the current code chunk, and the options will be restored after the chunk (thanks, @r2evans, #764)

- the argument `shortcode` in `knit2wp()` can take a logical vector of length 2 now; the first element determines whether to highlight source code, and the second decides syntax highlighting for text output (thanks, Derek Ogle, #663)

- Sweave2knitr() will report the line numbers indicating where the Sweave-specific syntax was used (thanks, Kirill Muller, #676)

- added a function `current_input()` to return the filename of the input document (thanks, Kate Davis, #701)

- for Markdown, if n (n >= 3) backticks are detected in the code output, the output will be wrapped in n+1 backticks to make sure the original backticks in the output are not interpreted as the token of fenced code blocks

- for a chunk hook function, we can use a fourth optional argument `name`, which takes the value of the hook name, e.g. for `knit_hooks$set(foo = hook_foo)`, `hook_foo` can be of the form `function(before, options, envir, name)`, where `name == 'foo'` (thanks, Thell Fowler, #733)

- all the four arguments `before`, `options`, `envir`, and `name` for a chunk hook are optional now, e.g. you can define a hook function of the form `function(before, options)`, `function(before, name, envir)`, and so on

- added two read-only chunk options `out.width.px` and `out.height.px`, which are the numbers of pixels calculated from `fig.width` and `fig.height` (which have units in inches), respectively, and the chunk options `out.width` and `out.height` can override the calculated natural size

- added a new argument `encoding` to the `pandoc()` function to specify the character encoding of the input file(s) (fixed the problem http://stackoverflow.com/q/22198832/559676)

## BUG FIXES

- due to the change in evaluate v0.5, evaluate() may return the raw values of expressions, but the S3 method wrap() does not know how to handle them; now these values are just ignored (thanks, Dan Tenenbaum)

- fixed a bug for dep_auto() that may occur if old cache files generated from previous versions of knitr are used (thanks, Jeffrey Racine)

- fixed the bug reported at http://stackoverflow.com/q/19166724/559676: the inline hook did not work well with non-numeric values, e.g. Date (thanks, Waldir Leoncio)

- infinity is formatted as a symbol in inline LaTeX and HTML modes (thanks, Kirill Müller, #629)

- `kable()` did not keep the row name when the data only has one row (thanks, @eev2, #636)

- `kable()` did not recycle the `align` argument correctly (thanks, Adam Cooper, #638)

- `kable()` chokes on tables with NA's (thanks, Harlan Harris and Benjamin Schiller, #720)

- `kable()` did not work with non-numeric data frames (thanks, @talexand, #702)

- for Markdown/reST tables, `kable()` should make sure there are at least one empty line before the table output (thanks, @talexand, #705)

- fixed a bug related to child documents -- chunk options passed from a parent document may not be restored after the child document exits (thanks, Frank Harrell, http://bit.ly/17yitsD)

- fixed a bug for `hook_rgl()` when the chunk option `dev` is a vector of length greater than one (thanks, Ashley Manton, #703)

- `hook_rgl()` did not work with the chunk option `fig.cap` (thanks, Kohske Takahashi, #700)

- `%\documentclass{}` was mistakenly treated as the specification of the document class instead of a comment in a LaTeX document (thanks, Jarad Niemi, #713)

## MAJOR CHANGES

- the default value for the chunk option `tidy` is `FALSE` now, which means the R source code in chunks will no longer be reformatted by `formatR::tidy.source()` by default; this feature must be explicitly turned on by `tidy=TRUE`, and it has brought a lot of confusion in the past, so it is perhaps a good idea not to reformat the source code by default

- now we treat the closing mark (e.g. `@` in Sweave or the three backticks in R Markdown) as part of the code chunk, instead of the beginning of a text chunk, and a consequence for this change is that **knitr** no longer adds blank lines to the beginning of the text chunks (thanks, Thell 'Bo' Fowler, #752)

- inline R expressions will no longer be evaluated in `try()`, which means errors in inline R code will be emitted immediately

- the first argument of the `plot` hook is the filename of the plot now; in previous versions, it was a vector of length 2 (basename and file extension); see `?hook_plot`

- the default value for the `format` argument in `pandoc()` has changed: it defaults to the `t` field in the configuration if found, otherwise, it defaults to `'html'` (thanks, Kohske Takahashi, #697)

- in the previous version, we can set `options(knitr.foo = value)` so that **knitr** can adjust the package options `opts_knit$set(foo = value)` before knitting a document; now the prefix for package options has been changed to `knitr.package.`, i.e. we should set `options(knitr.package.foo)` to achieve `opts_knit$set(foo)`; besides, it is also possible to change the default chunk options using `options(knitr.chunk.foo)` now, but you are warned that this may bring reproducibility issues, so please use with care

## MINOR CHANGES

- for R Markdown/AsciiDoc, line breaks are allowed in the inline R expressions now (thanks, Andrew MacDonald, #655)

- for AsciiDoc input, the R inline expressions can be written using the same syntax as R Markdown, i.e. `r expression`

- numbers containing characters other than digits 0-9, periods or commas are formatted as math in inline LaTeX, for negative numbers, infinity symbol, corner cases such as `10^{n}`, ... (thanks, Jeffrey Racine and Kirill Müller, #635)

- the column name `id` for row names in the `kable()` output is removed when the output format is Markdown (thanks, Artem Klevtsov, http://stackoverflow.com/q/19875529/559676)

- for R Markdown, special characters in figure filenames are no longer replaced by `_`, since RStudio v0.98.490 has fixed the base64 encoding bug; please upgrade RStudio to the latest version: http://www.rstudio.com/ide/download/

- the `tikzMetricsDictionary` option (for the **tikzDevice** package) is no longer overridden if it has already been set in `options()` (thanks, @rmatev, #708)

# CHANGES IN knitr VERSION 1.5

## NEW FEATURES

- a new option value `results='hold'` to flush all text output to the end of a chunk like `fig.show='hold'` (thanks, Harlan Harris, #593)

- when cache is enabled, automatic chunk dependencies can be truly automatic now; there is no need to call `dep_auto()` explicitly, and all we need to do is the chunk option `autodep=TRUE`; the chunk dependencies will be rebuilt after each chunk, so when new chunks are inserted into the document, **knitr** can also figure out the new dependencies automatically (thanks, @knokknok, #592)

- for Sublime Text users, there is a [SublimeKnitr](https://github.com/andrewheiss/SublimeKnitr) package to support LaTeX and Markdown with **knitr**; thanks, Andrew Heiss (#449) (this is not really a new feature of **knitr** itself, though)

- now the chunk options `warning` and `message` can also take numeric values as indices to select which warnings/messages to include in the output (thanks, Simon Urbanek, #590)

## BUG FIXES

- code changes in chunks should invalidate the cache when the chunk option `cache < 3`; fixed by @knokknok in #587

- fixed the bug reported at http://stackoverflow.com/q/18302179/559676; before evaluating inline R code, the chunk options `eval` must be evaluated to a logical value

- fixed the bug reported at http://stackoverflow.com/q/18544045/559676; read_chunk() no longer excludes code without chunk headers, so stitch() will include all the code as expected

- fixed the bug reported under http://stackoverflow.com/a/18541083/559676; now `\ensuremath{}` can be correctly applied to numbers of the form `10^{-n}`

- fixes a regression bug reported by Graham Williams: https://groups.google.com/forum/#!topic/knitr/_I5rlo9tOeA the global chunk options set in child documents are no longer restored

- fixed #604: `kable()` did not work on data frames/matrices of one row (thanks, Kevin Ushey)

- fixes the bug reported at http://stackoverflow.com/q/18992260/559676; `render_jekyll('prettify')` should have pasted the source code lines into one character string

## MAJOR CHANGES

- when the chunk option `cache=2`, the recorded plots (i.e. display lists) will no longer be cached, and the figure files will be assumed to exist the next time the document is compiled, otherwise the cache will be purged and the chunk needs to be recomputed; this will save some disk space and avoid bugs like #588 (thanks, @knokknok)

## MINOR CHANGES

- the Rnw chunk syntax is more tolerant on chunk headers in the sense that any characters after `<<>>=` are discarded, e.g. `<<echo=TRUE>>===foo` will be treated as `<<echo=TRUE>>=` (thanks, Michael Friendly)

- `knitr:::.onLoad()` no longer modifies the `PATH` variable when `/usr/texbin` is not in `PATH` under Mac OS

- when a message/warning/error contains line breaks, they are preserved and the message will not be re-wrapped (#602, thanks, Tyler Rinker)

- `read_chunk()` tolerates white spaces at the end of the chunk headers now (suggested by John Maindonald, #606)

- for R HTML documents, only the `highlight` component in `opts_knit$get('header')` was used in previous versions; now all components except `framed` and `tikz` are used; this makes it possible to further customize the HTML header (thanks, Wahlen Neuwirth and Erich Neuwirth)

- in previous versions, the global option `KNITR_WIDTH` in R was used to set `options(width)`; now this option is set from `opts_knit$get('width')`, which has the same default value `75` (#597)

# CHANGES IN knitr VERSION 1.4

## NOTE

- if you are using Windows or Linux, you need to read the news about both versions 1.3 and 1.4, because the version 1.3 did not manage to survive on CRAN

## NEW FEATURES

- the cache system is more granular now: instead of the binary option `cache = TRUE / FALSE`, we can specify `cache = 0, 1, 2, 3` (`TRUE` indicates 3, and `FALSE` indicates 0; for 1 and 2, see the documentation for `cache`: https://yihui.org/knitr/options); this means we may change the chunk options involved only with output rendering (e.g. from `echo = TRUE` to `FALSE`, or set `fig.cap = 'a new caption'`) without breaking the cache (thanks, Jeroen Ooms, Clark Kogan, and Roman Lustrik, #396, #536)

- added two new vignette engines called `docco_linear` and `docco_classic` using the Docco styles (http://jashkenas.github.io/docco/); see `browseVignettes(package = 'knitr')` for examples

- added a function `rocco()` to compile R Markdown documents to HTML using the classic Docco style, i.e. a two-column layout, with text on the left and code on the right (thanks, Weicheng Zhu, #577)

- added an argument `comment` in `spin()` to specify comment lines that will be ignored by `spin()`; by default, the block comment `/* comment */` is recognized; thanks, Bryan Hanson http://stackoverflow.com/q/17664401/559676

- it is possible to set package options prior to loading the **knitr** package now: for a package option `foo`, we can set `options(knitr.foo = value)` so that **knitr** will `opts_knit$set(foo = value)` when calling `knit()`; see `?opts_knit` for details (thanks, Zhiguang Zhao)

- added a new argument `ext` to the `pandoc()` function so that users can manually specify the output filename extensions (thanks, Baptiste, http://stackoverflow.com/q/17710249/559676)

- for LaTeX and HTML output, syntax highlighting can be done for languages besides R (e.g. Python, Perl, ...); this is achieved by `highr::hi_andre()`, so Andre Simon's Highlight must be installed, otherwise **knitr** will fall back to verbatim output for source code; see https://github.com/yihui/knitr-examples/blob/master/098-highlight-python.Rnw for an example (#495)

## MAJOR CHANGES

- **knitr** formally depends on the **highr** package now (for syntax highlighting of LaTeX and HTML)

- the package option `stop_on_error` has been deprecated; now it is much easier to specify whether you want to stop on errors or not by using the existing chunk option `error`; if you want to stop, use `error=FALSE`; see the documentation for details: https://yihui.org/knitr/options

- the meanings of the chunk options `warning` and `message` when they take the value `FALSE` have also changed: `FALSE` means the warnings/messages will be printed in the R console and not recorded; this makes it easier to know when/where the warnings/messages were produced during `knit()`

- syntax highlighting and adding prompts are done in the `source` hook now; in previous versions, they were done in `knitr:::wrap.source`; now the `source` hook receives the pure source code instead of syntax highlighted code

## MINOR CHANGES

- for the chunk options set in package option `opts_knit$get('eval.after')`, they will not be evaluated after a chunk if `eval=FALSE` for that chunk (#570, thanks, @knokknok)

- for document formats that produce HTML output, the default graphical device is changed to `png` only if it is `pdf`; if the device has been changed to values other than `pdf`, **knitr** will no longer modify it internally; when the `png` device is not available, the `svg` device will be used instead

- removed the global option `KNITR_PROGRESS`, which was introduced to suppress the progress bar, but now we have got `knit(..., quiet = TRUE)`, so this option is redundant

## BUG FIXES

- in LaTeX output, the double quotes `"` in the messages, errors, and warnings are replaced by `"{}` because they might cause trouble to babel: http://stackoverflow.com/q/18125539/559676 (thanks, Thierry)

# CHANGES IN knitr VERSION 1.3

## NEW FEATURES

- added support for AsciiDoc; see example 089 at https://github.com/yihui/knitr-examples/blob/master/089-minimal.Rasciidoc (thanks, Richard Cotton)

- added support for reStructuredText vignettes in packages; now `*.Rrst` documents are recognized as vignettes as well, and they will be compiled to PDF via `rst2pdf` (#533) (thanks, Trevor L Davis)

- a new function `kable()` to produce simple tables; see the vignette `datatables` in `browseVignettes(package = 'knitr')` for an application; also see a LaTeX example at https://github.com/yihui/knitr-examples/blob/master/091-knitr-table.Rnw

- the chunk options `fig.width` and `fig.height` are vectorized according to the `dev` option, e.g. `fig.wdith = c(7, 10)` and `dev = c('pdf', 'png')` (#538) (thanks, @baptiste)

- for `purl()`, code chunks with the option `purl=FALSE` will not be included in the R script (#519, thanks, Sebastian)

- new `'hide'` value to the `fig.show` option; the figures are created but not included in the output document (#532) (thanks, Simon)

- the `sas` engine uses the listings output now (#541) and was tweaked for better LaTeX display (#562) (thanks, Nick Salkowski)

- added a quick and dirty `c` engine (via `R CMD SHLIB`); see https://github.com/yihui/knitr-examples/blob/master/090-engine-c.Rmd for an example

- added a new engine `asy` for Asymptote, a vector graphics language (http://asymptote.sourceforge.net); see examples 093 at https://github.com/yihui/knitr-examples (thanks, Thibaut Lamadon, #559)

- added a new engine `cat` to write the content of a code chunk to a file; see the example 095 at https://github.com/yihui/knitr-examples (thanks, Stephen Eglen)

- added a new function `knit_exit()` to allow `knit()` to exit early as if it had reached the end of the document (#524, thanks, Renaud)

- the chunk option `fig.align` also works for Markdown output now, e.g., `fig.align = 'center'` will center images in HTML via the `style` attribute of the `<img>` tag (#387)

- the argument `format` in the `pandoc()` function was vectorized, e.g. we can call `pandoc(input, format = c('html', 'latex'))` and the input file will be converted to HTML and LaTeX, respectively (#547, thanks, Jeroen Ooms)

- added an argument `options` to `knit_child()` to set global chunk options for child documents; if a parent chunk calls a child document (via the `child` option), the chunk options of the parent chunk will be used as global options for the child document, e.g. for `<<foo, child='bar.Rnw', fig.path='figure/foo-'>>=`, the figure path prefix will be `figure/foo-` in `bar.Rnw`; see http://stackoverflow.com/q/17514055/559676 for an application

- `eclipse_theme()` works with font weight (bold) and font style (italic) now when parsing themes from http://eclipsecolorthemes.org

- added two package options `latex.options.graphicx` and `latex.options.color` to allow customization of LaTeX package options, e.g. `opts_knit$set(latex.options.color = 'monochrome')` generates `\usepackage[monochrome]{color}` in the LaTeX output (#546)

- added a new package option `unnamed.chunk.label` to set the chunk labels for unnamed chunks, e.g. `opts_knit$set(unnamed.chunk.label = 'fig')` will generate chunk labels `fig-1`, `fig-2`, ... (#555) (thanks, Noam Ross)

- when `knit()` is running, a global option `knitr.in.progress` will be set to `TRUE`; this allows other package authors (e.g. `rCharts` and `googleVis`) to adjust certain package options according to `getOption('knitr.in.progress')` (thanks, Ramnath V, #564)

## BUG FIXES

- fixed #502: using `layout()` and `par()` at the same time under R 3.0.0 may lead to a corrupt plot (thanks, Hong Xu http://tex.stackexchange.com/q/108335/9128)

- fixed a bug in `pandoc()`: for single-lettered Pandoc arguments, the values are passed to them after spaces instead of equal signs (reported at http://stackoverflow.com/q/16569010/559676)

- fixed #542: when a child document has a sub-child document and also uses `set_parent()`, the LaTeX header will be added to the wrong file (thanks, Johan Toloe)

- `stitch_rmd()` was using a wrong R Markdown template

- fixed #537: misleading error message when the graphical device does not exist (thanks, Scott Kostyshak)

- fixed a bug in `hook_rgl()` reported at http://cos.name/cn/topic/110742 (incorrect LaTeX code when `fig.align='center'` and `fig.show='hold'`)

- fixed #565: added the `envir` argument to `spin()` so that the code is evaluated in the correct environment by default (thanks, @GillesSanMartin)

## MAJOR CHANGES

- `purl()` will write the chunk headers in `# ---- label, opts ----` instead of `# @knitr label, opts`, i.e. `@knitr` was changed to four dashes `----`; similarly, `read_chunk()` will use the pattern `# ---- label ----` to read external code (the old syntax `# @knitr` is still preserved for compatibility); the main reason for this change is that RStudio uses four dashes in R comments as section headings, so that it is possible to fold sections of R

- syntax highlighting is done by the **highr** package if it has been installed, otherwise, the old regular-expression-based syntax highlighting will still be used; the **highr** package does much better syntax highlighting than the regexp-based approach (#327)

- the commands for syntax highlighting were changed for compatibility with Andre Simon's Highlight package; this will affect LaTeX and HTML users, e.g. `\hlnumber` was renamed to `\hlnum`; cached LaTeX and HTML will have to be rebuilt for the new syntax highlighting commands to work (#470)

- the argument `eval` was removed in `knit_child()`; if we do not want to evaluate a child document, we can set `eval=FALSE` in its parent chunk

- the script `inst/bin/knit` gains an option `-o` to specify the output filenames for `knit()` (#525, thanks, Aaron Wolen)

- the default video format for animations is OGG (it is open and free) instead of MP4 (non-free) now; this means Internet Explorer under Windows may not work with the animations (consider Firefox, Chrome and other modern web browsers)

- warnings and messages in adjacent output chunks are merged, respectively (#534)

- when the package option `verbose = TRUE`, the time stamp will be printed after each chunk using `timestamp()`, but this will mess up with the R command history, so now **knitr** uses `cat()` to write the time stamp (#545) (thanks, @knokknok)

- the argument `base` in `read_rforge()` was removed and hard-coded inside the function instead

- for Markdown output, the figure filenames no longer allow special characters like spaces, and special characters will be automatically replaced by `_`; this change will avoid problems when publishing to RPubs from RStudio: if figure paths contain special characters, the figures will not be uploaded (thanks, Sangsoon Woo)

## MINOR CHANGES

- the package vignettes uses `\VignetteEngine{knitr::knitr}` instead of `\VignetteEngine{knitr}` so that the next version of R can compile the vignettes out of the box (via `R CMD Sweave`) and no longer need to build the whole package in order to build the vignettes

## MAINTENANCE

- the package vignettes were moved to the `vignettes` directory from `inst/doc` since the former will be preferred by the future versions of R

- the testing is done via the **testit** package now (https://cran.r-project.org/package=testit)

## MISC

- **knitr** becomes an affiliated project of FOAS (Foundation for Open Access Statistics): http://www.foastat.org/projects.html

# CHANGES IN knitr VERSION 1.2

## NEW FEATURES

- added a Pandoc wrapper function `pandoc()` to convert Markdown documents to other formats that Pandoc support such as LaTeX/PDF, OpenDocument, HTML5 slides and unfortunately also Word (#206)

- in the past, the chunk hooks were ignored when the chunk option `engine != 'R'` (i.e. code in the chunk is not R); now they are executed regardless of the language of the code chunk (#483) (thanks, @cdrv)

- multiple WebGL objects from the **rgl** package can be generated in the same web page now; each object is uniquely identified by the corresponding chunk label; see https://dl.dropbox.com/u/15335397/misc/webgl-rmd.html for an example (#494) (thanks, Carson Sievert)

- if multiple graphical devices are used (the chunk option `dev` is a vector of length greater than 1), the chunk option `dev.args` can be a named list of lists of device arguments, with each element a list of arguments to be passed to the single device; see https://yihui.org/knitr/options

- as announced in the last version, R 3.0.0 will support non-Sweave vignettes; now it is also possible to compile R HTML vignettes via **knitr** since `*.Rhtml` files are also registered by **knitr** as vignette files

- a new chunk option `cache.vars` to manually specify which variables to save in the cache database; by default all newly created and modified variables are identified and saved, but in some cases, **knitr** may not be able to identify the modified variables, e.g. `DT[, foo:=value]` in **data.table** (we can set `cache.vars='DT'` to force **knitr** to save a copy of `DT`)

- added a new engine `Rscript` to run the R code in a new R session; see http://stackoverflow.com/q/15271406/559676 for an example

- the executable script `inst/bin/knit` can accept multiple input files now (e.g. `knit foo.Rnw bar.Rmd zzz.Rhtml`)

- `knit()` and `knit2html()` gained a `quiet` argument to suppress messages and the progress bar (thanks, Vince Buffalo)

- added the `text` argument to `spin()` and `stitch()` respectively as an alternative way to provide the input like `knit()` (#509) (thanks, Craig Watson)

- a new function `wrap_rmd()` to wrap long lines in Rmd files without affecting the code blocks (if there are any); this makes it easier for version control purposes

- `rst2pdf()` will pass a default output filename to `rst2pdf` (if the input is `foo.rst`, the output will be `foo.pdf`)

- `knit2wp()` gained a new argument `publish = TRUE` (thanks, Eric Nantz) (#512)

## BUG FIXES

- fixed the problem reported in http://stackoverflow.com/q/12448507/559676 now \usepackage{upquote} should appear after \usepackage{fontenc}, so single quotes in verbatim environments will no longer cause problems

- fixed #487: `stitch_rhtml()` and `stitch_rmd()` should not use the chunk option `out.width = '.6\\linewidth'` (thanks, Tal Galili)

- when the chunk option `engine` is not `R`, the code is also executed under the directory `opts_knit$get('root.dir')` (if specified); thanks, Winawer http://stackoverflow.com/q/15512545/559676

- `:` is permitted in `fig.path` now (#513) (thanks, Sebastian)

- fixed an encoding problem (`CP950`) for Hong Kong Windows users reported at http://bit.ly/16RQL5E

## MAJOR CHANGES

- all child documents are inserted into the parent document as character strings of the (compiled) content, instead of being saved into files (e.g. `\input{foo-child.tex}`); no matter how many child documents there are, only one main output file will be generated; the package option `child.command` was removed accordingly since it is no longer used

- no longer generates concordance data for child documents; the past attempt did not really work well and the implementation was complicated, so now we only support concordance for the main document; the consequence of this change is the synchronization between PDF and Rnw for child documents no longer works at the line level (clicking in PDF will still bring the focus back to the child chunk)

- in previous versions, cached chunks were evaluated in separate (empty) environments in order to capture the newly created variables, but this brings confusion when we use functions depending on the current environment such as `ls()` (which will return `character(0)`); now all chunks, cached or not, are evaluated in the same environment `knit_global()` (finally fixed #456)

- `knit2pdf()` and `knit2html()` return the output filename when the input is a file (in previous versions, `NULL` was returned in this case)

- the package option `stop_on_error` is set to `2` now when building package vignettes, which means R will stop on errors in vignettes; this makes it easier to find out possible problems in vignettes during `R CMD build`

- the document hook `hook_rjournal()` was removed; it was too hackish (see https://yihui.org/en/2013/02/contribute-to-the-r-journal-with-lyx-knitr/ for how to write an article for The R Journal in a less hackish way)

## MINOR CHANGES

- the progress bar symbol was changed from `>` to `.` so it looks less intrusive (#395) (thanks, Michael Friendly)

## DOCUMENTATION

- the **knitr** book is forthcoming: http://www.crcpress.com/product/isbn/9781482203530 run `citation('knitr')` or `toBibtex(citation('knitr'))` in R to obtain the citation info

- open `help(package = 'knitr', help_type = 'html')` to see the vignette examples (Rnw, R Markdown and R HTML)

# CHANGES IN knitr VERSION 1.1

## NEW FEATURES

- (experimental) R 3.0.0 will support non-Sweave vignettes, e.g. Rnw documents can be compiled by **knitr** instead of Sweave; in addition, R Markdown vignettes have also become first-class citizens as R package vignettes; see https://yihui.org/knitr/demo/vignette/ for details

- a new engine for CoffeeScript (i.e. the chunk option `engine='coffee'`); see https://github.com/yihui/knitr-examples/blob/master/080-engine-coffeescript.Rmd for an example (thanks, Nacho Caballero)

- when the chunk option `eval=FALSE`, `purl()` will comment out the code when extracting code chunks (thanks, Randall Pruim)

- the global option `KNITR_PROGRESS` can be used to set the package option `progress` in `opts_knit`, e.g. after `options(KNITR_PROGRESS = FALSE)`, this option will be set to `FALSE` when the package is loaded (#395)

- the global option `KNITR_WIDTH` can be use to set the R option `width` before **knitr** is called; in the past this option was hard-coded to `75`, and now it is set as `options(width = getOption('KNITR_WIDTH', 75L))`

- a new function `knit2wp()` which compiles R Markdown documents and publishes the results to WordPress; see https://yihui.org/knitr/demo/wordpress/ for details

- a new hook `hook_webgl()` which writes the WebGL code of an **rgl** scene into the output using `rgl::writeWebGL()` so we can reproduce a 3D plot in the browser (thanks, Stephane Laurent http://stackoverflow.com/q/14879210/559676)

## BUG FIXES

- fixed #465: when `eval=FALSE` and `echo` is numeric, the code was incorrectly filtered by the indices in `echo` (thanks, @ateucher)

- `>` was not allowed in HTML inline code expressions (http://stackoverflow.com/q/14360296/559676); now the regular expression works correctly to look for `-->` instead of `>`

- `set_parent()` should not print `NULL` in the child document: http://stackoverflow.com/q/14487718/559676 (thanks, Thomas Holz)

- child documents now inherit the encoding parameter from their parent document, i.e. the `encoding` value in `knit(..., encoding = ?)` is applied to `knit_child()`; note this assumes the parent document and the child documents use the same encoding (thanks, Henrik Nyhus)

## MAJOR CHANGES

- empty inline expressions are no longer recognized, e.g. `\Sexpr{}` will not be parsed; this allows one to write such markers in tutorials (e.g. `<!--rinline -->` and `` `r ` ``); internally `all_patterns$foo$inline.code` was changed

- the function `build_dep()` was removed (the warning has been there for a long time); please use `dep_auto()` instead

- the package option `filter.chunk.end` was removed; this means in Rnw documents, a single line `@` has the meaning of terminating a chunk _only if_ there is a chunk header `<<>>=` before it; otherwise it does not have any special meanings

- the function `run_chunk()` was removed; it is redundant because we already have the chunk option `ref.label` as well as in-chunk reference `<<label>>`

## MINOR CHANGES

- the function `imgur_upload()` uses Imgur API version 3 now; if you are using the key obtained from version 2, you need to register for your own client id: http://api.imgur.com (#439)

- allow users to pass a custom environment to `Rcpp::sourceCpp()` in the `Rcpp` engine; fixes http://stackoverflow.com/q/14882486/559676

- slight improvement of encoding support in `knit()`

# CHANGES IN knitr VERSION 1.0

## NEW FEATURES

- a new function `knit_expand()` which is similar to **brew** and mustache, e.g. it expands `pi is {{pi}}` to `pi is 3.14`; it can also be used for building child documents (see https://github.com/yihui/knitr-examples/blob/master/075-knit-expand.Rnw for example) (#397) (thanks, Frank Harrell)

- `knit()` gained a new argument `encoding` to specify the encoding of the input file (multilingual support is complete now), e.g. `knit(..., encoding = 'GBK')` for Simplified Chinese

- a new function `Sweave2knitr()` to convert Sweave documents to **knitr**; several automatic translations can be done, e.g. `results=tex` to `results='asis'`, `width=5` to `fig.width=5`, `echo=true` to `echo=TRUE`, `keep.source=TRUE` to `tidy=FALSE`, `eps=TRUE` to `dev='postscript'`, `\SweaveOpts{...}` to `opts_chunk$set(...)` and so on; see the documentation in the package for details (#451)

- if the Sweave syntax is detected in an Rnw document, a message box (**tcltk**) will pop up as a reminder on how to fix it

- inline R code also respects the option `opts_knti$get('stop_on_error')` now, e.g. if we set this option to `2L`, R will completely stop when error occurs in inline R code

- a new function `all_labels()` to get all chunk labels in a document; see one application at https://github.com/yihui/knitr-examples/blob/master/073-code-appendix.Rnw

- chunk hooks will be run (if exist) even if the code chunk is empty

- added two wrapper functions `stitch_rhtml()` and `stitch_rmd()` which use the R HTML and R Markdown templates respectively when calling `stitch()`

- the chunk label is used as the id of the div element in R HTML output, e.g. `<div id='chunk-label'>...</div>`

## MAJOR CHANGES

- (IMPORTANT) the internal compatibility with Sweave has been dropped as scheduled in the last version, and the **knitr** parser was greatly simplified accordingly; Sweave users can call the function `Sweave2knitr()` to convert old Rnw files to **knitr** files before running `knit()`

- accordingly, the pattern elements `global.options` and `inline.doc` were removed from `knit_patterns` (`\SweaveOpts{}` and `\SweaveInput{}` will no longer be supported; please call `Sweave2knitr()` to convert incompatible Sweave documents)

- chunk labels can be arbitrary characters now; in the past they had to be valid R symbols, e.g. `2a` was an invalid label; this restriction has been removed, because chunk labels will be automatically quoted when necessary (`<<2a>>=` will become `<<'2a'>>=`, but `<<'3-function'>>=` will remain untouched)

- if the chunk option `include=FALSE`, the evaluation will stop if errors occur (i.e. `stop_on_error=2L` for **evaluate**) because otherwise, it will be very difficult for authors to notice errors in chunks which have `include=FALSE` (#453) (thanks, Joshua Pritikin)

- the function `knit_env()` is no longer available (it is not exported any more), and `knit_global()` has been exported now

## MINOR CHANGES

- for inline R code, the value is returned only if the R code prints a visible value, e.g. `\Sexpr{x <- 1}` will be empty, and `\Sexpr{pi}` will return the value of `pi`

## BUG FIXES

- fixed #432: no longer uses `\\\\` in LaTeX output; only a single line break is converted to `\\` (thanks, Kevin Wright)

- `render_html()` guarantees that the R source code is highlighted when the chunk option `highlight = TRUE` (#447) (thanks, Ramnath Vaidyanathan)

- `dep_auto()` was unable to find the cache files if the input document is not under the current working directory (thanks, Hui Yao)

## Documentation

- because Github has deprecated downloads, all downloads were moved to Bitbucket, and the links in the package website, as well as all examples, have been updated (#438)

# CHANGES IN knitr VERSION 0.9

## NEW FEATURES

- added a demo named `notebook` which is an R notebook based on the **shiny** package (https://github.com/rstudio/shiny); use `demo('notebook', package = 'knitr')` to see it, or visit http://glimmer.rstudio.com/yihui/knitr

- for numeric inline output in LaTeX, the `I()` trick is no longer needed, e.g. `$x=\Sexpr{1.2e10}$` is safe for LaTeX now due to `\ensuremath{}` (#137) (thanks, Randall Pruim)

- the chunk option `eval` can take numeric values now, specifying which expressions to evaluate in a chunk (#383) (thanks, Jared Lander)

- a new package option `stop_on_error` which specifies the behavior of errors in code chunks; this option is passed to the **evaluate** package, e.g. `opts_knit$set(stop_on_error = 2L)` will make **knitr** completely stop on errors (the default value is `0L` which means to move on even if errors occurred); this makes it possible to see the call stacks via `traceback()` in an interactive R session when an error occurs (#344) (thanks, Hadley Wickham and Dzidorius Martinaitis)

- added support to the **Rcpp** package through the chunk option `engine='Rcpp'` so that we can write C++ source code in the document; see https://github.com/yihui/knitr-examples/blob/master/029-engine-Rcpp.Rmd for an example (#415) (thanks, JJ Allaire)

- **knitr** throws a warning when a cached chunk depends on an uncached chunk because this kind of dependency will be ignored (#431) (thanks, @ghostwheel)

- a list of arguments can be passed to `formatR::tidy.source()` as the chunk option `tidy.opts` now, e.g. `tidy.opts=list(width.cutoff=60, keep.blank.line=FALSE)` (#429)

- some chunk options are recycled for plots such as `fig.env`, `out.width` and `out.extra`, etc; this means if there are multiple plots per chunk, we can specify different output options for them individually (e.g. `out.width=c('2in', '.4\\linewidth')` for two plots); see https://github.com/yihui/knitr-examples/blob/master/067-graphics-options.Rnw for an example (motivated by #430) (thanks, @autumnlin)

- added a new chunk option `fig.subcap` for captions of subfigures in LaTeX; when there are multiple plots in a chunk, and neither `fig.subcap` nor `fig.cap` is NULL, `\subfloat{}` will be used for individual plots (you need to add `\usepackage{subfig}` in the preamble); also see https://github.com/yihui/knitr-examples/blob/master/067-graphics-options.Rnw for an example (#388) (thanks, @skipperhoyer)

- `stitch()` accepts labelled R scripts now; if an R script contains chunk headers of the form `## @knitr label, options`, they will be used in the template (#411) (thanks, @jamiefolson)

- the function `read_chunk()` gained a few new arguments so that we can reference external code chunks in another way, which was an idea from the **SweaveListingUtils** package (thanks, Peter Ruckdeschel)

- a new function `read_demo()` based on `read_chunk()` to read demo scripts in R packages

- a new convenience function `read_rforge()` to read code from R-Forge repositories; combined with `read_chunk()`, it can insert R code from R-Forge into **knitr** dynamically; see https://github.com/yihui/knitr-examples/blob/master/046-read-rforge.Rmd for an example (thanks, Peter Ruckdeschel)

- chunk options are also written after `## @knitr` along with chunk labels when tangling R scripts via `purl()`

- `purl()` gained a new argument `documentation` to also write documentation lines into the R script (#391 and #401) (thanks, Noam Ross and Fernando Mayer)

- `knit_rd()` generates a navigation frame on the left and builds links now; this is like the CHM help in old days (thanks, Michael Friendly)

- a new function `knit_rd_all()` to build static HTML help pages for all the packages installed

- we can also use `## @knitr` to write chunk options for `spin()` now (`#+` and `#-` still work)

- added new language support for Perl and Z Shell (`zsh`); see an example at https://github.com/yihui/knitr-examples/blob/master/028-engine-perl.Rmd (#406) (thanks, Jim Hester)

- `render_jekyll()` gained an argument `highlight` to specify which highlighting engine to use (Pygments or Prettify.js) (thanks, Yanping Chen)

- two new chunk options for language engines: `engine.path` and `engine.opts`; the former can be used to specify the path of the program (e.g. `<<engine='ruby', engine.path='/usr/bin/ruby1.9.1'>>=`); the latter can be used to pass additional arguments to the engine program

- added new engines for GraphViz (`engine='dot'`) and TikZ (`engine='tikz'`); see https://github.com/yihui/knitr-examples/blob/master/057-engine-dot.Rmd and https://github.com/yihui/knitr-examples/blob/master/058-engine-tikz.Rmd for examples (#419) (thanks, Michel Kuhlmann)

- added a preliminary engine for SAS which is basically a call like `system('sas chunk-code.sas')` (#354)

- a new `document` hook to post-process the LaTeX output document to move code chunks out of figure/table environments so that code chunks will not float with the environments; see `?hook_movecode` for details

- chunk hooks are called in the _reverse_ order after a chunk (and natural order before a chunk); this allows one to, e.g. write an opening environment before a chunk and close it properly after a chunk

- all language engines also respect the `comment` option when writing output just like R code chunks (by default the output is commented out by `##`)

- added a new function `set_alias()` as a wrapper to `opts_knit$set(aliases = ...)`, e.g. `set_alias(w = 'fig.width')` sets `w` as an alias for the chunk option `fig.width`

## MAJOR CHANGES

- global options are strongly recommended to be set via real R code `opts_chunk$set(opt = value)` in a code chunk instead of the old syntax in text chunks like `\SweaveOpts{opt = value}`, or `<!--roptions opt=value-->`, etc, which will be deprecated in the next version; this will make it cleaner and safer to parse the source documents, e.g. we can write arbitrarily complicated expressions like `opts_chunk$set(fig.width = if (foo == 'beamer') { 5 } else { 7 })` which is impossible in the old syntax; if you still use the old syntax like `\SweaveOpts{}`, you will see a warning with a pause of 10 seconds

- based on the same reason, it is recommended to use the chunk option `child` to input child documents; old syntax like `\SweaveInput{}` will be deprecated

- for markdown output, results from inline R code will no longer be put in a pair of backticks (#379)

- the package option `opts_knit$get('cache.extra')` was removed because this option should really be a chunk option instead; see https://yihui.org/knitr/demo/cache/ for the updated documentation (#404 and #405) (thanks, Jim Hester)

- the chunk option `highlight.opts` was deprecated and renamed to `engine.opts`; this affects users who use Andre Simon's highlight through the `highlight` engine in **knitr**

- the chunk option `file` for Awk was deprecated; we can also use `engine.opts` to specify the file for Awk; see https://github.com/yihui/knitr-examples/blob/master/024-engine-awk.Rmd for example

- the pattern `knit_pattern$get('ref.label')` was deprecated since it is unlikely to be customized; a fixed pattern `'^#+\\s*@knitr(.*)$'` will be used instead

## MINOR CHANGES

- when `opts_knit$get('verbose')` is `TRUE`, a `timestamp()` will be printed before each code chunk is evaluated (#377) (thanks, Carl Boettiger)

- `stitch()` will no longer copy the template over to the current working directory (thanks, Michael Friendly)

- `stitch()` will no longer open the PDF/HTML output automatically (#411) (thanks, Michel Kuhlmann)

- the script `inst/bin/knit` can also convert R Markdown documents to HTML now; the argument `--pdf` was removed and a new argument `--no-convert` was added

- dots in figure filenames will not be replaced with `_` when the output is not LaTeX (thanks, Stewart Macarthur)

## BUG FIXES

- fixed #410: when the inline R code returns `NA_real_`, the scientific notation of numbers will run into errors (thanks, Rafik)

- the syntax pattern for Rnw documents was not quite right: `all_patterns$rnw$chunk.end = '^\\s*@\\s*%*'` actually allows any characters after `@`, but only LaTeX comments and white spaces are allowed; it has been fixed to `^\\s*@\\s*(%+.*|)$` now

## DOCUMENTATION

- an example of combining R, knitr and D3 to draw a contour plot: https://yihui.org/knitr/demo/javascript/

# CHANGES IN knitr VERSION 0.8

## NEW FEATURES

- output from other languages (e.g. python, awk, ...) can also be cached like R output when `cache=TRUE`; see [023-engine-python.Rmd](https://github.com/yihui/knitr-examples/blob/master/023-engine-python.Rmd) for an example

- added support for bash/shell scripts; use the chunk option `engine='bash'` or `engine='sh'` to write shell scripts in code chunks (#361)

- a new function `knit_rd()` to knit package documentation (run examples code and insert output in the HTML documentation) (#227) (thanks, Taiyun Wei)

- added LuaTeX support for tikz graphics with the **tikzDevice** package (set `options(tikzDefaultEngine = 'luatex')`); this feature requires **tikzDevice** version > 0.6.2 (#358) (thanks, Alastair Andrew)

- a new chunk option `fig.env` to set which environment to use for figures in LaTeX, e.g. we can set `fig.env='marginfigure'` to use `\begin{marginfigure}` (#364) (thanks, Bryan Hanson)

- added a new package option `global.device` (default `FALSE`) which specifies whether to use a global graphics device to capture plots; if `TRUE`, it is possible to write `plot(1:10)` in a previous chunk and `abline(0, 1)` in a latter chunk because all code chunks share the same device, however, this may also bring unexpected consequences (in particular, using `par()` can bring redundant plots)

## BUG FIXES

- dots in figure paths are more safely replaced with `_` now, e.g. `fig.path='../figure'` will no longer be replaced by `__/figure` (#346) (thanks, @ralfer)

- the `c()` syntax for the chunk option `dependson` did not actually work, e.g. `dependson=c('foo', 'bar')` (#350) (thanks, Cassio Pereira)

- fixed a bug when `eval=FALSE` and `prompt=TRUE` (the continuation character was used in some places where there should be the prompt character) (thanks, Derek Ogle)

- `persp()` plots were not recognized in the last version (thanks, Jeffrey Racine)

## MAJOR CHANGES

- leading spaces are allowed in chunk headers now, e.g. in the past `<<>>=` must appear in the beginning of a line, but now we can indent the chunk header by a number of white spaces; this amount of spaces will be stripped off the chunk if the whole chunk is indented (#236) (thanks, @jamiefolson and Vitalie Spinu)

- markdown output will be indented if the original code chunk is indented; this allows chunk output to be nested within its parent environment, e.g. inside an ordered list (see [001-minimal.Rmd](https://github.com/yihui/knitr-examples/blob/master/001-minimal.Rmd) for example)

- when the global chunk option `eval=FALSE`, inline R code will not be evaluated, and `??` is returned for inline R expressions (#367)

## MINOR CHANGES

- if `getOption('OutDec')` is not `.`, inline numeric output will be put inside `\text{}` in LaTeX to avoid situations like #348 (the normal math mode may add a space after the comma in `3,1415`)

- if the chunk option `external==FALSE` (default is `TRUE`), **knitr** will no longer automatically add `\usepackage{tikz}` to the LaTeX preamble; you need to add it manually (but it is recommended to use `external=TRUE` with `cache=TRUE` for the sake of speed, because compilation of tikz graphics may be slow)

- `*.brew` generates `*.txt` by default (instead of `*-out.brew`)

- `knit(text = ...)` will no longer write output in the console (the output is only returned as a character string)

## DOCUMENTATION

- added a simple reference card: `vignette('knitr-refcard', package = 'knitr')`

# CHANGES IN knitr VERSION 0.7

## NEW FEATURES

- added a new chunk option `out.extra` to write extra graphics output options, e.g. `<<out.extra='angle=90'>>=` to rotate the figure by 90 degrees; see https://yihui.org/knitr/options (#301) (thanks, @knokknok)

- when `opts_knit$get('verbose')` is TRUE, logs (messages, warnings and errors) along with the corresponding R code will be printed after `knit()` is done; this might help users figure out possible problems in R code quickly (#276)

- `.Random.seed` is cached again for the sake of reproducibility; see https://yihui.org/knitr/demo/cache/ for how to maintain reproducibility when the computation involves with random number generation (#274) (thanks, Renaud)

- the package option `opts_knit$get('cache.extra')` can be an unevaluated R expression now, e.g. `opts_knit$set(cache.extra = quote(.Random.seed))`; see the cache page above for a concrete example

- added a new package option `'root.dir'` (default `NULL`) which can be used to set the root directory to evaluate code chunks in a document; by default, the root directory is the directory of the input document, and this option enables users to set other directories as the working directory for code chunks (#277) (thanks, Ken Williams)

- `spin()` will add `\documentclass{article}` for Rnw output if no document class is specified in the R script so that the LaTeX output will be a complete document (#295) (thanks, Christiaan Klijn)

- added Ruby support in the `engine` option; see the example https://github.com/yihui/knitr/blob/master/inst/examples/knitr-lang.Rmd (#294) (thanks, Ramnath Vaidyanathan)

- also added Haskell support in the option `engine='haskell'` through calling `ghc` (#336) (thanks, Michel Kuhlmann)

- added support to Andre Simon's `highlight` through the option `engine='highlight'`; see https://gist.github.com/3114112 for an example of highlighting Matlab code in LaTeX (thanks, Dirk Eddelbuettel and Renaud Gaujoux)

- the output hooks for LaTeX, HTML, Markdown and reST will respect the `engine` option now, so these hooks can be readily used for output when the language is not R, e.g. `render_markdown(strict = TRUE)` also works for Python output (#251) (thanks, Chris Fonnesbeck)

- the chunk options `eval`, `echo` and `results` are also respected when the language is not R, e.g. for a Python code chunk with `eval=FALSE`, the code will not be evaluated, or for a Ruby chunk with `results='hide'`, the output will be hidden (#293) (thanks, Ramnath Vaidyanathan)

- chunk options `out.width`, `out.height` and `out.extra` also work for plots in HTML and Markdown output now, e.g. `out.width='200px'` or `out.extra='style="display:block;"'` (#297) (thanks, Taiyun Wei and Alan Severini)

- the hook function to create animations in HTML output is exported now as `hook_ffmpeg_html()`

- added a package option `opts_knit$get('animation.fun')` which defaults to `hook_ffmpeg_html`; this option is used to create animations in HTML output so that we do not have to use FFmpeg

- added two functions `hook_r2swf()` and `hook_scianimator()` which can be set as the package option `opts_knit$get('animation.fun')` and create animations via the **R2SWF** package or the **SciAnimator** library (see `animation::saveHTML`) (thanks, Taiyun Wei)

- a new function `image_uri()` to create data URIs for image files; we can set `opts_knit$set(upload.fun = image_uri)` so that images are embedded in the HTML output as data URIs (hence the HTML page does not depend on external images) (#298, #324) (thanks, Wush Wu)

- added a new object `opts_template` which can be used to set a group of chunk options and they can be referenced later with the new chunk option `opts.label`; see `?opts_template` for examples; this makes it easy to reuse groups of frequently used chunk options (#316, #320) (thanks, Cassio Pereira)

- a new function `dep_prev()` to build chunk cache dependencies so that all later chunks will depend on previous chunks; if any of a previous chunk is updated, the cache of all chunks after it will be updated as well (#285) (thanks, @muelleki)

- a new chunk hook function `hook_optipng()` to optimize PNG images using `optipng` (#272) (thanks, Winston Chang)

- added a new output hook named `document` in `knit_hooks` (see `knit_hooks$get('document')`); this hook function is used to process the output of the whole document; it can be useful when we want to post-process the whole output before writing it to the output file

- a new function `rst2pdf()` which uses the program `rst2pdf` to convert reST documents to PDF; it is also supported by `knit2pdf()` when `compiler='rst2pdf'` (#300) (thanks, Alex Zvoleff)

## BUG FIXES

- fixed #286: messages (including warnings and errors) are guaranteed to be ended by `\n`, so even when chunk option `comment=NA`, messages will also be rendered correctly (thanks, Carl Boettiger)

- fixed #273: when knitting a file under another directory with cache turned on (e.g. `knit('foo/bar.Rnw')`), `lazyLoad()` is unable to load the cache files under a relative path because the working directory has been changed to the directory of the input file during the evaluation

- fixed #292: layout() may cause the capture of unwanted plots (thanks, Austen Wallace Head)

- fixed #302: when there are multiple instances of `\documentclass` in the source document, **knitr** will be unable to insert the LaTeX preamble

- fixed #308: if `options('OutDec')` was set to a character other than `.`, the LaTeX code can be malformed (#308) (thanks, Cassio Pereira)

- `opts_chunk$set()` in a child document was only working in that child document, but was expected to change chunk options globally; now it works everywhere, and will affect all chunks after this setting, no matter where `opts_chunk$set()` is (thanks, Guy Lebanon) (http://bit.ly/MexHXd)

- fixed #332: calling `purl()` inside a source document when `knit()` the same document could cause clashes; now it is safe to put `purl()` inside a source document and `knit()` it

- fixed #342: when `eval=FALSE`, line breaks in the source code were missing

## MAJOR CHANGES

- if the chunk label contains non-alphanumeric characters (except `-` and `_`), these characters will be replaced by `_` in the figure filenames (if there are any) and a warning will be issued; this is to guarantee the figure filenames are valid to LaTeX (#321) (thanks, (Roman Lustrik)

- the [**highlight**](https://cran.r-project.org/package=highlight) package is not enabled by default; use `opts_knit$set(use.highlight = TRUE)` to enable it

- the default LaTeX output will put messages, warnings and errors in special LaTeX environments: errors are red, warnings are magenta, and messages are italic; in previous versions, they were in the `verbatim` environment (#264) (thanks, @muelleki)

- unnamed chunks are named sequentially in a single call of `knit()` according to the order of their appearance no matter where they are, e.g. if there are two unnamed chunks in two child documents respectively, they will be named `unnamed-chunk-1` and `unnamed-chunk-2`; in previous versions, both will be named as `unnamed-chunk-1` which can cause clashes of cache and figure files

- the function `build_dep()` was renamed to `dep_auto()` which better reflects what this function really does; it is still available in this package but may be removed in future versions

- the package **tikzDevice** was removed from the `Suggests` field, but this will not affect users who have already installed **tikzDevice**; for those who have not, this package has been archived on CRAN (hopefully only temporarily), so you have to install from the source

## MINOR CHANGES

- the LaTeX environment `kframe` was updated so that it can be used in other environments such as `center` or `tabular` (#283) (thanks, @muelleki)

- the OpenCPU demo is more robust to double quotes in the text (#271); see http://public.opencpu.org/userapps/opencpu/knitr/

- for Sweave output, the results will not be put inside the Schunk environment when `results='asis'`

- `stitch()` templates use more sensible figure paths by default: the path for figures is `'figure/input-script-name'` now, i.e. it will be different for different input R scripts to avoid possible clashes

- `stitch()` templates no longer use default title and author names; if the user did not set them in the R script (as meta comments `# title:` and `# author:`), there will not be titles or authors in the output

- **knitr** will no longer use scientific notations for integers in inline R code output; sci notation only applies to double-precision numbers (#296) (thanks, @knokknok)

- `options()` set in the main document will apply to its child documents (e.g. `options('digits')`) (#306) (thanks, Cassio Pereira)

- the `...` argument in `knit2html()` is passed to `markdown::markdownToHTML()` instead of `knit()`; this allows us to pass more arguments to control the rendering of HTML output, e.g. `knit2html(input, fragment.only = TRUE)` (#333) (thanks, @Bart6114)

## DOCUMENTATION

- the example using other languages was updated to show how some chunk options (`eval`, `echo` and `results`) also work for foreign languages: https://github.com/yihui/knitr/blob/master/inst/examples/knitr-lang.Rmd

# CHANGES IN knitr VERSION 0.6.3

## MAJOR CHANGES

- this is an urgent patch version for CRAN: the dependencies on **highlight** and **parser** were removed because these two packages were orphaned on CRAN; now **knitr** uses a naive syntax highlighter for LateX and HTML output if **highlight** is not available, which has a similar appearance with **highlight**; when the **parser** package is not available, users should be careful with the chunk option `tidy=TRUE`: `replace.assign` may not work as expected; see the NEWS of **formatR** for details: https://github.com/yihui/formatR/blob/master/NEWS; you are welcome to improve the naive highlighter: https://github.com/yihui/knitr/tree/master/R/highlight.R

## NEW FEATURES

- `spin()` gained a new argument `report` for Rnw/Rtex/Rmd documents to be compiled to PDF/HTML respectively (#287) (thanks, kohske takahashi)

## BUG FIXES

- `stitch()` and `spin()` should use `parent.frame()` to evaluate code by default; in 0.6, the default parent frame for the inner `knit()` was a wrong environment to use (thanks, Michael Nelson)

- use `unnamed-chunk-i` as the label when chunk `label == ''` (#280) (thanks, Josh Paulson)

- fixed #279 and #281, both of which are about concordance

- `'\\maxwidth'` does not apply to LaTeX/PDF animations, so the default value of `out.width` for animations in LaTeX is still `NULL`; you will have to set `out.width` manually for a reasonable width (#282) (thanks, Ramnath Vaidyanathan)

## MISC

- **knitr** and RStudio had an invited talk at useR! 2012; slides at https://yihui.org/slides/2012-knitr-RStudio.html (source at https://github.com/yihui/knitr-talks)

# CHANGES IN knitr VERSION 0.6

## NEW FEATURES

- for LaTeX output, the chunk option `out.width` defaults to `'\\maxwidth'` now; see https://yihui.org/knitr/demo/framed/ for its definition; this makes sure the figures do not overflow the page margins (#221)

- the chunk option `size` now defines the font size of the whole chunk instead of only some special characters in it, so that the old trick of redefining the `knitrout` environment for different font sizes is no longer necessary; see updates in the beamer examples: https://yihui.org/knitr/demo/beamer/ (thanks, Baptiste Auguie)

- added a new chunk option `dev.args` which can be used to pass more arguments to the graphical device (#254) (thanks, knokknok)

- warnings, messages and errors will be wrapped according to `options('width')`; this will make long messages stay within the page margin when the width option is appropriately small (#259) (thanks, @muelleki)

- added a new function `spin()` to turn a specially formatted R script to a report; see https://yihui.org/knitr/demo/stitch/ (#223) (thanks, Rich FitzJohn)

- `knit()` gained a new argument `envir` to specify the environment in which to evaluate code chunks (by default in the parent frame); this will allow users to run code in a specified environment so that the global environment will not be cluttered by objects in chunks, which can hopefully make the documents more reproducible (#228)

- added a new component `inline.comment` in `knit_patterns` to strip off tokens of inline R code from inline comments, e.g. a comment line like `% x is \Sexpr{x}` will become `% x is x` (#110); this will only happen to LaTeX documents because HTML does not have inline comments (it only has block comments `<!-- -->`)

- concordance works for child documents as well now (try RStudio), although it is not very precise (#225); note when concordance is enabled, the results from child documents will be written into the parent output instead of individual tex files; also note you have to set `opts_knit$set(self.contained = FALSE)` for concordance to work better

- added an OpenCPU app so that we can run knitr in the cloud now; see `system.file('opencpu', 'apps', 'index.html', package = 'knitr')` or http://public.opencpu.org/apps/knitr (thanks, Jeroen Ooms)

- all messages, errors and warnings during evaluation are recorded in an internal object `knit_log` (use `knitr:::knit_log$get()` to get all messages) and they are printed if the package option `verbose` is `TRUE` (i.e. `opts_knit$get('verbose')`) (#224)

- child documents are also supported in other document formats besides LaTeX, e.g. Markdown and HTML, etc; please use the chunk option `child` to specify the child documents; see `system.file('examples', 'child', 'knitr-main.Rmd', package = 'knitr')` for an example in markdown (#268) (thanks, @martinaryee)

## BUG FIXES

- the templates for `stitch()` used `results=hide` which should really be `results='hide'` (#219) (thanks, @r2d3)

- format numbers with the reST syntax instead of HTML (#218) (thanks, Jeffrey Arnold)

- `hook_pdfcrop()` should work better under Windows now (#209) (thanks @DCCKC and @r2d3)

- tikz compilation fails on Windows network drives

- FFmpeg does not really work for HTML/Markdown output because the dot in figure filenames was omitted (thanks, Ming Kuo)

- child documents can fail when they are in different subdirectories (thanks, Christoph J)

- `set_parent()` failed to work in the last version due to a bug when inserting the parent preamble into the child document (#240)

- better preserve plot sizes in interactive sessions (#258)

## MAJOR CHANGES

- `.Random.seed` is not cached any more because of weird problems due to lazy loading (#248 and #253); users should use `set.seed()` to make sure reproducibility of random simulations; the chunk output is cached in a `.RData` database instead of a lazy-load database to avoid problems in #253

- the default graphics device is set to the null PDF device before evaluating code chunks, i.e. `pdf(file = NULL)`, so that neither `Rplots.pdf` nor plot windows will be opened during knitting

## MINOR CHANGES

- **knitr** will show a message when a chunk is empty; this helps users who do not actually want a chunk to be empty realize the problem like #229; in the past, knitr just silently returns an empty string from such chunks

- **knitr** will show a message when the cache is loaded and the option `opts_knit$get('verbose')` is `TRUE` (#249) (thanks, Carl Boettiger)

- the filename extensions `Snw` and `Stex` are also supported (`foo.Snw`/`foo.Stex` produces `foo.tex`)

- the HTML output hooks are changed according to the suggestion of Barry Rowlingson (#250) and the default CSS is also slightly modified

- `knit()` will no longer try to remove the file `NA` generated by `pdf(file = NULL)` before R 2.14.2 (which was a bug and fixed later); you should update R if you see this file

## DOCUMENTATION

- added a minimal brew example under `system.file('examples', package = 'knitr')`

# CHANGES IN knitr VERSION 0.5

## NEW FEATURES

- white spaces are allowed before `<<>>` when using chunk references, and this approach of references is supported in tex and html documents as well

- added a new pattern list named `md` so that R code blocks can be written more naturally in extended markdown (e.g. GFM and pandoc): use ```` ```{r label, opts}```` to begin a chunk and ```` ``` ```` (three or more backticks) to begin normal text, and write inline R code in `` `r code.here` ``; it is the default syntax for markdown input, or you can call `pat_md()` before `knit()` so **knitr** can make use of this pattern list to process the input document

- RStudio has added full support to **knitr**: we can knit HTML and Markdown documents easily now, and markdown output will be automatically converted to an HTML preview page just like TeX to PDF

- if the pattern list is not set in advance, **knitr** will try to detect the syntax automatically by matching all built-in pattern lists against the input document, e.g. if the input contains `<<>>=`, the Rnw syntax will be used, and if ```` ```{r} ```` is detected, the markdown syntax will be used; this feature enables us to use different sets of syntax freely, e.g. we can use Sweave syntax in markdown files and knitr will be able to recognize it (#189)

- new filename extensions with a prefix `R` or `r` are introduced: `*.Rhtml`, `*.Rhtm`, `*.Rtex`, `*.Rmd` and `*.Rmarkdown` will be recognized as source documents like `*.Rnw`, and the output filename will be automatically determined by removing the `R` prefix, e.g. `foo.Rmd` produces `foo.md` by default; the old clumsy naming convention `foo_knit_.md` is still preserved but not recommended any more (#187)

- new function `knit2html()` to knit an Rmd file (R with markdown) and convert to HTML in one step using the **markdown** package

- new functions `pat_rst()` and `render_rst()` to support reStructuredText; use `.. {r label, options}` and `.. ..` to write R code chunks; see https://github.com/yihui/knitr/tree/master/inst/examples/knitr-minimal.Rrst (thanks, Jeffrey Arnold and Ramnath Vaidyanathan)

- new package option `self.contained` which decides whether to write style definitions (highlighting) in external files or put them in the output document; the highlighting definitions in LaTeX output is often too long, so `opts_knit$set(self.contained = FALSE)` can help in this case (#176) (thanks, Ramnath Vaidyanathan)

- new package option `filter.chunk.end` which decides if the `chunk.end` pattern really means `chunk.end`; see https://yihui.org/knitr/options (thanks, Joe Cheng)

- syntax highlighting themes are also available to HTML output now; the usage is the same as in LaTeX (#179) (thanks, Ramnath Vaidyanathan)

- the chunk option `fig.cap` is also used in markdown output now

- the random seed `.Random.seed` is also cached for the sake of reproducibility in random simulations

- the function call `read_chunk()` will be evaluated when tangling R code via `purl()` (#175) (thanks, Carl Boettiger)

- the default LaTeX output will use the **upquote** package if it exists so that single quotes are straight in the output (thanks, Mathematical Coffee http://bit.ly/IKjluw)

- the chunk option `engine` is back but it is used to run code from other languages instead of just ignoring the code in chunks which have `engine != 'R'`; currently this option is still in rough edges and only supports python and awk; other languages may be added in future versions, but users can also do it by themselves by `knit_engines$set(language = function(options) {...})`; see an example at `system.file('examples', 'knitr-lang.Rmd')` (#201)

- new function `write_bib()` to write Bibtex citations for R packages; see the main manual for a sample usage (#13)

- `hook_pdfcrop()` also supports cropping other image formats like PNG or JPEG through ImageMagick (`convert -trim`) (#203) (thanks, @r2d3)

## MAJOR CHANGES

- **knitr** will completely stop when duplicated labels are found and the corresponding chunks are non-empty; in the previous version, only a warning is given and R code in later chunks will override previous chunks (#185) (thanks, Randall Pruim)

- the default graphical device for HTML and markdown output is `png()` now to avoid the possible unexpected consequence that PDF plots are included in an HTML page which will not work normally in browsers

- markdown output will use the extended markdown hooks by default now: `render_markdown(strict = FALSE)`; in the past the default was `render_jekyll()`; the major difference is that the code blocks are put in ```` ```r and ``` ````; if you want the strict markdown syntax, you can all `render_markdown(strict = TRUE)` which will indent code blocks by 4 spaces

- `render_gfm()` has been removed because the name can be misleading; the main purpose of this function was to put code blocks in ```` ``` ````, and now we can replace it by `render_markdown(FALSE)`; other markdown flavors also support such fenced code blocks (e.g. pandoc) -- it is not limited to Github only

- the default value for the `fig.path` option is `'figure/'` now so that plots will be put under this directory by default; the default was `'./'` in the past which makes the current directory messy when there are many plots

- **knitr** will fully stop when an error is encountered in `knit()`; in the past, only a message was issued in this case in an interactive R session

- the package option `all.patterns` has been dropped; please use the objects `all_patterns` or `knit_patterns` directly if you want to tweak the syntax

## BUG FIXES

- the compilation of tikz graphics can hang up when there are TeX errors in the tikz file; now we use `\nonstopmode` to avoid hiccup (#188)

- multiple devices per chunk was buggy (#181)

- S4 objects will be printed by `show()` instead of `print()`; this is a bug of the **evaluate** package, which has been fixed (please update it to be > 0.4.2)

## MISC

- it is recommended to use `opts_chunk$set()` to set global chunk options now instead of `\SweaveOpts{}`; all documentation has been updated (#216)

- number of downloads (https://github.com/yihui/knitr/downloads) of **knitr** documentation before I removed and updated them on GitHub: c(main = ?, graphics = 275+)

# CHANGES IN knitr VERSION 0.4

## NEW FEATURES

- Sweave concordance was finally implemented: when `opts_knit$get('concordance')` is `TRUE`, **knitr** will write a file named `'input-concordance.tex'` which contains the mapping between input Rnw and output tex line numbers; this feature is mainly for (but not limited to) RStudio to provide better error navigations: you can jump from the TeX error message to the Rnw source directly to know where the error comes from (the line number of the source of the error may not be accurate but should be in the ballpark) (#133) (thanks, JJ Allaire and Joe Cheng)

- if output hooks have been set before calling `knit()`, they will be respected, i.e. **knitr** will no longer override them by default hooks; you need to make sure *all* output hooks are set appropriately, e.g. you can start by `render_latex()` and change some individual hooks later (#165) (thanks, Andrew Redd)

- newly created objects in the global environment will also be cached if cache is turned on (`cache=TRUE`); in previous versions **knitr** is unaware of objects created in `globalenv()`, e.g. `setGeneric()` creates S4 generic functions in `globalenv()` and **knitr** was unable to capture them (#138) (thanks, syoh)

- chunk options `dev`, `fig.ext` and `dpi` can be vectors now; this allows one to save a plot to multiple formats, e.g. `<<foo, dev=c('pdf', 'png')>>=` creates two files for the same plot: `foo.pdf` and `foo.png` (#168) (thanks, MYaseen208)

- an experimental feature for animations created by FFmpeg in HTML/markdown output when `fig.show='animate'` (#166) (thanks, gabysbrain)

- the chunk option `fig.cap` supports multiple figure captions in LaTeX now, e.g. if a chunk produces two plots, we can use `fig.cap = c('first caption', 'second caption')` to assign two different captions to them respectively when `fig.show = 'asis'` (#155) (thanks, Jonathan Kennel)

- new package option `opts_knit$get('upload.fun')` which is a function that takes a plot file to upload to a certain host and returns the link to the image; by default it is `imgur_upload()`, and you can use your own function to upload images to other hosts like Flickr (#159) (thanks, Carl Boettiger)

- all packages loaded in the current session are also cached, so as long as a package has been loaded previously, it will be available to all following chunks (#160)

- new chunk option `autodep` and function `build_dep()` to build cache dependencies among cached chunks automatically by analyzing object names in all cached chunks; this is a loose alternative to the `dependson` option (see main manual and `?build_dep` for details) (#72) (thanks, Seth Falcon)

- input and output in `knit()` are no longer restricted to files; they can be `stdin()`/`stdout()` or other types of connections (#162; see https://github.com/yihui/knitr/issues/162) (thanks, gabysbrain)

- as-is output (`results='asis'`) and plots are no longer put in the framed environments because of incompatibilities (#163) (thanks, DCCKC, Murray Logan and Jennym Hutchison)

## BUG FIXES

- for plots in LaTeX output, centering should be done with `{\centering }` instead of `\centering{}` (#156) (thanks, Ramnath Vaidyanathan)

- the recorded plot is a more precise representation of the expected plot now because the recording device also takes the plot size into consideration (#157) (thanks, Christiaan Klijn and Frank Harrell)

- `format_sci()` now correctly formats 0; this function is used for inline R code to format numbers in scientific notation (#161) (thanks, Kihoro J. M.)

- fixed a bug for the case in which the chunk option only contains the label like `<<label=foobar>>=`; knitr 0.3 was unable to parse the label correctly (`<<foobar>>=` is OK) (thanks, Muhammad Yaseen)

## MINOR CHANGES

- `imgur_upload()` returns the link to the image directly, with the XML list as its attribute (in v0.3 the list was returned)

- more verbose messages in case of chunk errors: both line numbers of the source and chunk info will be printed

## DOCUMENTATION

- website updated as usual: https://yihui.org/knitr

- added an example for the subfloat environment: https://github.com/yihui/knitr/releases/download/doc/knitr-subfloats.pdf

- most manuals (main or graphics manuals) have been updated

## MISC

- number of downloads (https://github.com/yihui/knitr/downloads) of knitr documentation before I removed and updated them on GitHub: c(main = 400, graphics = 177)

# CHANGES IN knitr VERSION 0.3

## NEW FEATURES

- a fundamental and important new feature for writing chunk options: they can be written as valid R code now, just like we write function arguments (e.g. `echo=c(1, 3, 5)`, `fig.cap="my figure caption"`); all options will be parsed and evaluated as R code by default; see https://yihui.org/knitr/options for details (#142) (thanks, Baptiste Auguie)

- chunk references using `<<label>>` is supported now (#86); thanks to Kevin R. Coombe and Terry Therneau for the discussion

- new function `run_chunk()` to run the code in a specified chunk, which is an alternative to the chunk reference in Sweave; see https://yihui.org/knitr/demo/reference/

- a executable script `knit` under `system.files('bin', package = 'knitr')` which makes it easier to call knitr via command line under *nix (call `knit input [output] [--pdf]`)

- the inline hooks respect `getOption('digits')` and `getOption('scipen')` now (see `?options`); numbers returned from inline R code will be formatted according to these two options (see a demo at https://yihui.org/knitr/demo/output/)

- if you still use old Sweave syntax for chunk options, it is possible to write literal commas in chunk options now -- they have to be escaped by `\`, e.g. `caption=hello\, world`; this will be parsed to `'hello, world'` as a character string; of course this looks ugly and has limited power, so please please consider the new syntax!

- `knit2pdf()` gained another argument `compiler` which can be used to specify the program to compile the tex document to PDF, such as xelatex (#131) (thanks, Ramnath Vaidyanathan and Dennis Murphy)

- a new function `imgur_upload()` to upload images to imgur.com; it can be used in HTML or Markdown hooks so the output is a self-contained document which does not need additional image files; `opts_knit$get('upload.fun'`) can use this function (#66) (thanks, Ramnath Vaidyanathan)

- a child document can be compiled individually with the LaTeX preamble borrowed automatically from a parent document using a new function `set_parent()`; see the help page for details (#136) (thanks, Helder Correia)

- to avoid `$$` around numbers in the inline output, we can use `I()` to protect the numeric inline output, e.g. `$x = \Sexpr{I(10^7)}$` gives `$x = 10^7$` whereas `\Sexpr{10^7}` gives `$10^7$` (thanks, Kevin Middleton)

- the listings package is formally supported now (see `?render_listings`); the default style is borrowed from `Sweavel.sty` written by Frank Harrell (#101) (thanks, Frank)

- new package option `cache.extra` which allows more objects to affect cache; see https://yihui.org/knitr/demo/cache/ (#134)

- new package option `child.path` to specify the search path of child documents relative to the parent document (#141)

- new package option `aliases` to set aliases for chunk options; see https://yihui.org/knitr/options (#144)

- new chunk options `fig.cap`, `fig.scap` and `fig.lp` to write captions, short captions, label prefix for the figure environment in LaTeX (#145) (thanks, Frank Harrell)

- new package option `eval.after` to set a character vector of chunk options which should be evaluated _after_ a chunk is executed (thanks, Frank Harrell)

- a series of convenience functions `pat_rnw()`, `pat_tex()`, `pat_brew()` and `pat_html()` to set built-in patterns (syntax) to read input

## MINOR CHANGES

- package option `eval.opts` has been dropped: all options of classes `symbol` or `language` will be evaluated, so there is no need to specify which options to evaluate manually; remember, the chunk options are similar to function arguments, so you can use any valid R code there

- the default value for the `output` argument in `knit()` is NULL now, so we can also provide output filenames to `stitch()` and `knit2pdf()` (#119)

- standard LaTeX messages are suppressed when a tikz plot is compiled to PDF so that we can see the **knitr** process more clearly

- `%\SweaveInput{}` will be ignored now (#150)

- `results=asis` will no longer affect the `chunk` hook (in the past, the chunk output was not wrapped in the `kframe` environment when `results=asis`); it only affects the `output` hook now

- the package website allows comments now

## MAJOR CHANGES

- the starting pattern of normal texts in an Rnw document is `^@\\s*%*` instead of `^@\\s*$` now, meaning you can write `@ % a comment` to end a code chunk (this is consistent with Sweave)

- the default value of the argument `output` of `knit()` will be a filename under the current working directory; in previous versions, the output file will be under the same directory as the input file; this change makes it possible to completely separate the input files and output files into different places, and hopefully will give users better experience in managing a whole collection of files (including child documents): put all source files in one place and output files in another place

- the package homepage is https://yihui.org/knitr now (the previous URL yihui.github.com/knitr will be automatically redirected to the new address)

## BUG FIXES

- the object `opts_current` does not give the evaluated version of the current chunk options because it was created before the options are evaluated; this has been fixed and `opts_current$get()` will give the expected values of options (thanks, Frank Harrell)

## MISC

- number of downloads (https://github.com/yihui/knitr/downloads) of knitr documentation before I removed and updated them on GitHub: c(main = 1300, graphics = 549, themes = 130, beamer = 565, listings = 240, minimal = 160)

# CHANGES IN knitr VERSION 0.2

## NEW FEATURES

- added support for including child documents in the main document (like `\SweaveInput{}` but with different implementations); see https://yihui.org/knitr/demo/child/ (#92)

- for inline R code, character results are returned as-is now (without `\texttt{}`)

- new function `purl()` as a wrapper to `knit(..., tangle = TRUE)` which extracts R code from the input document (thanks to Dieter Menne's wife who suggested the function name)

- the error hook applies to inline R code when an error occurs in the inline R code, in which case knitr will not stop by default; instead, it writes the error message into the output (#85)

- chunk option `split` also works for HTML output now using `<iframe></iframe>` (#82)

- `knit()` gained an argument `text` as an alternative to `input` (#88)

- new chunk option `child` to include child documents into the main document (#92)

- chunk option `external` defaults to `TRUE` now (was `FALSE` by default in the last version)

- added a new demo to show how to build package vignettes with knitr: https://yihui.org/knitr/demo/vignette/

- added support to the `quartz()` device under Mac (#103); now the `dev` option has more choices (see https://yihui.org/knitr/options)

- chunk option `echo` can take a numeric vector to select which R expressions to echo into the output now (#108); see https://yihui.org/knitr/options

- a new function `stitch()` which is a convenience function to insert an R script into a template and compile (to quickly create a report
  based on an R script)

- for a chunk hook to run, the corresponding chunk option no longer has to be `TRUE`; it can be any non-null values; this enables us to make use of the option value directly instead of only knowing it is `TRUE` (see https://yihui.org/knitr/demo/cache/ for an example)

- `knit()` will no longer write figure or cache files in the same directory as the input document; instead, these files are written in the current working directory (see ?knit)

- a new function `knit_env()` that makes the environment of the current chunk accessible to the user

## BUG FIXES

- the code used to merge global chunk options and local options was buggy for cache; it has been fixed now, so cache is more stable (#105), but users may see previously cached chunks being re-evaluated with this version, which should be regarded as a normal phenomenon, and on the second run, the cached chunks will not be evaluated again

- fixed a buglet when using both options `out.width` and `out.height` in Rnw (#113)


# CHANGES IN knitr VERSION 0.1

## NEW FEATURES

- first version of knitr: it covers most features in Sweave, **cacheSweave** and **pgfSweave**; see package homepage for documentation and examples: https://yihui.org/knitr/

## MISC

- **knitr** won an Honorable Mention prize (before it was formally released to CRAN) in the Applications of R in Business Contest hosted by Revolution Analytics: http://bit.ly/wP1Dii http://bit.ly/wDRCPV

- in this NEWS file, #n means the issue number on GitHub, e.g. #142 is https://github.com/yihui/knitr/issues/142
