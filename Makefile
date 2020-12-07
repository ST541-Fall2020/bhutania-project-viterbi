all: main doc Final_Report.pdf project_walkthrough.pdf

main:
		Rscript -e 'main.R';

doc:
		Rscript -e 'devtools::document()'

Final_Report.pdf:	./doc/Final_Report.Rmd
		Rscript -e 'rmarkdown::render("./doc/Final_Report.Rmd")'

project_walkthrough.pdf: ./doc/project_walkthrough.Rmd
		Rscript -e 'rmarkdown::render("./doc/project_walkthrough.Rmd")'

clean:
		rm -f ./doc/project_walkthrough.pdf ./doc/Final_Report.pdf
