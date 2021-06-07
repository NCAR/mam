
VERSION = 0.0.1

GRAPHVIZ_FILE = mam_modules.gv
GRAPHVIZ_PDF = $(patsubst %.gv,%.pdf,$(GRAPHVIZ_FILE))
DOC_INDEX = output/html/index.html

ALL_SRC = $(wildcard ../test/integration/current_cam/*.F90)

.PHONY: all
all: doc

.PHONY: doc
doc: $(GRAPHVIZ_PDF) $(DOC_INDEX)

$(DOC_INDEX): $(ALL_SRC) $(GRAPHVIZ_PDF) Doxyfile
	doxygen

$(GRAPHVIZ_FILE): $(ALL_SRC) f90_mod_deps.py
	echo "digraph mam_modules {" > $@
	echo "    rankdir = TB;" >> $@
	echo "    node [fontsize = 10, height = 0.3, width = 0.5];" >> $@
	echo "    graph [nodesep = 0.2, ranksep = 0.3];" >> $@
	echo "    edge [arrowsize = 0.7];" >> $@
	./f90_mod_deps.py -D "\1" -M "\1" -v -g -i $(ALL_SRC) >> $@
	echo "}" >> $@

%.png: %.gv
	dot -Tpng $< > $@

%.eps: %.gv
	dot -Tps $< > $@

%.pdf: %.eps
	epstopdf $<

clean:
	rm -rf output $(GRAPHVIZ_FILE) $(GRAPHVIZ_PDF)
