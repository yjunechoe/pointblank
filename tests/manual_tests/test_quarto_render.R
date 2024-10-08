# Ensure that rendering reports in Quarto do not produce `data-qmd` attributes
# since the reports are not *data* tables

quarto::quarto_render("tests/manual_tests/test_quarto_render.qmd")
stopifnot(file.exists("tests/manual_tests/test_quarto_render.html"))
utils::browseURL("tests/manual_tests/test_quarto_render.html")

test_qmd <- xml2::read_html("tests/manual_tests/test_quarto_render.html")

data_qmd_divs <- xml2::xml_find_all(test_qmd, "//div[@data-qmd]")
data_qmd_divs

stopifnot(length(data_qmd_divs) == 0)

unlink("tests/manual_tests/test_quarto_render.html")
unlink("tests/manual_tests/test_quarto_render_files/*", recursive = TRUE)
