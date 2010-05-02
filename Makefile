#### Blackthorn -- Lisp Game Engine
####
#### Copyright (c) 2007-2010, Elliott Slaughter <elliottslaughter@gmail.com>
####
#### Permission is hereby granted, free of charge, to any person
#### obtaining a copy of this software and associated documentation
#### files (the "Software"), to deal in the Software without
#### restriction, including without limitation the rights to use, copy,
#### modify, merge, publish, distribute, sublicense, and/or sell copies
#### of the Software, and to permit persons to whom the Software is
#### furnished to do so, subject to the following conditions:
####
#### The above copyright notice and this permission notice shall be
#### included in all copies or substantial portions of the Software.
####
#### THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
#### EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
#### MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
#### NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
#### HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
#### WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#### OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
#### DEALINGS IN THE SOFTWARE.
####

# Search PATH for a Lisp compiler.
ifneq ($(shell which sbcl),)
	cl := sbcl
else
ifneq ($(shell which alisp),)
	cl := allegro
else
ifneq ($(shell which clisp),)
	cl := clisp
else
ifneq ($(shell which ecl),)
	cl := ecl
else
ifneq ($(shell which ecl.exe),)
	cl := ecl
else
ifneq ($(shell which ccl),)
	cl := clozure
else
	$(error No Lisp compiler found.)
endif
endif
endif
endif
endif
endif

# Which ASDF system to load:
system := thopter

# Stardard drivers:
prop := property.lisp
load := load.lisp
test := test.lisp
dist := dist.lisp
prof := profile.lisp
atdoc := atdoc.lisp

# Select which driver to run (load by default).
driver := ${load}

args :=

# Specify the run command for the installer.
ifeq (${cl}, allegro)
	command := \\\"\\x24INSTDIR\\\\main.exe\\\"
else
	command := \\\"\\x24INSTDIR\\\\chp\\\\chp.exe\\\" \\\"\\x24INSTDIR\\\\main.exe\\\"
endif

# A temporary file for passing values around.
tempfile := .tmp

# A command which can be used to get an ASDF system property.
ifeq (${cl}, allegro)
	get-property = $(shell alisp +B +s ${prop} -e "(defparameter *driver-system* '|${system}|)" -e "(defparameter *output-file* \"${tempfile}\")" -e "(defparameter *output-expression* '$(1))")
else
ifeq (${cl}, sbcl)
	get-property = $(shell sbcl --eval "(defparameter *driver-system* \"${system}\")" --eval "(defparameter *output-file* \"${tempfile}\")" --eval "(defparameter *output-expression* '$(1))" --load ${prop})
else
ifeq (${cl}, clisp)
	get-property = $(shell clisp -x "(defparameter *driver-system* \"${system}\")" -x "(defparameter *output-file* \"${tempfile}\")" -x "(defparameter *output-expression* '$(1))" -x "(load \"${prop}\")")
else
ifeq (${cl}, ecl)
	get-property = $(shell ecl -eval "(defparameter *driver-system* \"${system}\")" -eval "(defparameter *output-file* \"${tempfile}\")" -eval "(defparameter *output-expression* '$(1))" -load ${prop})
else
ifeq (${cl}, clozure)
	get-property = $(shell ccl --eval "(defparameter *driver-system* \"${system}\")" --eval "(defparameter *output-file* \"${tempfile}\")" --eval "(defparameter *output-expression* '$(1))" --load ${prop})
endif
endif
endif
endif
endif

# Get ASDF system properties for the specified system.
define get-properties
	$(eval temp := $$(call get-property,(asdf:component-name *system*)))
	$(eval name := $$(shell cat $${tempfile}))
	$(eval temp := $$(call get-property,(or (asdf:component-property *system* :long-name) (asdf:component-name *system*))))
	$(eval longname := $$(shell cat $${tempfile}))
	$(eval temp := $$(call get-property,(asdf:component-version *system*)))
	$(eval version := $$(shell cat $${tempfile}))
	$(eval temp := $$(call get-property,(asdf:system-description *system*)))
	$(eval description := $$(shell cat $${tempfile}))
	$(eval temp := $$(call get-property,(asdf:component-property *system* :url)))
	$(eval url := $$(shell cat $${tempfile}))
endef

export cl, db, system, driver, name, longname, version, description, url, command

.PHONY: new
new:
	$(MAKE) clean
	$(MAKE) load

.PHONY: load
load:
	$(MAKE) load-${cl}

.PHONY: load-allegro
load-allegro:
	alisp +B +s ${driver} -e "(defparameter *driver-system* '|${system}|)" -- ${args}

.PHONY: load-sbcl
load-sbcl:
	sbcl --eval "(defparameter *driver-system* \"${system}\")" --load ${driver} -- ${args}

.PHONY: load-clisp
load-clisp:
	clisp -x "(defparameter *driver-system* \"${system}\")" -x "(load \"${driver}\")" -- ${args}

.PHONY: load-ecl
load-ecl:
ifneq ($(shell which ecl.exe),)
	ecl.exe -eval "(defparameter *driver-system* \"${system}\")" -load ${driver} -- ${args}
else
	ecl -eval "(defparameter *driver-system* \"${system}\")" -load ${driver} -- ${args}
endif

.PHONY: load-clozure
load-clozure:
	ccl --eval "(defparameter *driver-system* \"${system}\")" --load ${driver} -- ${args}

.PHONY: server
server:
	$(MAKE) args="--server=127.0.0.1 --port=12345" new

.PHONY: client
client:
	$(MAKE) args="--connect=127.0.0.1 --port=12345" new

.PHONY: server3
server3:
	$(MAKE) args="--server --port=12345 --players=3" new

.PHONY: server4
server4:
	$(MAKE) args="--server --port=12345 --players=4" new

.PHONY: server5
server5:
	$(MAKE) args="--server --port=12345 --players=5" new

.PHONY: slime
slime:
	$(MAKE) clean
	emacs --eval "(progn (slime '${cl}) (while (not (slime-connected-p)) (sleep-for 0 200)) (slime-interactive-eval \"(defparameter *driver-system* \\\"${system}\\\")\") (slime-load-file \"${driver}\"))"

.PHONY: thopter
thopter:
	$(MAKE) system="thopter" new

.PHONY: bunny
bunny:
	$(MAKE) system="bunnyslayer" new

.PHONY: stress
stress:
	$(MAKE) system="blackthorn-stress-test" new

.PHONY: collision
collision:
	$(MAKE) system="blackthorn-collision-test" new

.PHONY: test
test:
	$(MAKE) driver="${test}" system="blackthorn-test" new

.PHONY: prof
prof:
	$(MAKE) driver="${prof}" new

.PHONY: distnoclean
distnoclean:
	mkdir -p bin
	cp -r $(wildcard lib/*) disp sound bin
	$(MAKE) driver="${dist}" new

.PHONY: dist
dist:
	$(MAKE) distclean distnoclean

.PHONY: README
README:
	pdflatex README.tex
ifneq ($(shell which hevea.bat),)
	hevea.bat -o README.html README.tex
	d2u README.html
else
	hevea -o README.html README.tex
endif

.PHONY: doc
doc:
	$(MAKE) docclean
	$(MAKE) atdoc

.PHONY: atdoc
atdoc:
	$(MAKE) driver="${atdoc}" new

.PHONY: install-w32
install-w32:
	-$(call get-properties)
	$(MAKE) distclean
	mkdir -p bin
	if test -e windows/${name}.ico; then cp windows/${name}.ico bin/app.ico; else cp windows/bt.ico bin/app.ico; fi
	$(MAKE) distnoclean
	cp -r windows/chp windows/is_user_admin.nsh COPYRIGHT bin
	awk "{gsub(/@NAME@/, \"${name}\");print}" windows/install.nsi | awk "{gsub(/@LONGNAME@/, \"${longname}\");print}" | awk "{gsub(/@VERSION@/, \"${version}\");print}" | awk "{gsub(/@DESCRIPTION@/, \"${description}\");print}" | awk "{gsub(/@URL@/, \"${url}\");print}" | awk "{gsub(/@COMMAND@/, \"${command}\");print}" > bin/install.nsi
	makensis bin/install.nsi
	mv bin/*-install.exe .

.PHONY: install-unix
install-unix:
	-$(call get-properties)
	$(MAKE) dist
	cp unix/run.sh bin
	mv bin ${name}
	tar cfz ${name}-${version}-linux.tar.gz ${name}
	mv ${name} bin

.PHONY: install-mac
install-mac:
	-$(call get-properties)
	rm -rf "${longname}.app"
	$(MAKE) dist
	mkdir "${longname}.app" "${longname}.app/Contents" "${longname}.app/Contents/MacOS" "${longname}.app/Contents/Frameworks" "${longname}.app/Contents/Resources"
	awk "{gsub(/@NAME@/, \"${name}\");print}" macosx/Info.plist | awk "{gsub(/@LONGNAME@/, \"${longname}\");print}" | awk "{gsub(/@VERSION@/, \"${version}\");print}" | awk "{gsub(/@DESCRIPTION@/, \"${description}\");print}" | awk "{gsub(/@URL@/, \"${url}\");print}" > "${longname}.app/Contents/Info.plist"
	cp -r $(wildcard lib/*.framework) "${longname}.app/Contents/Frameworks"
	cp -r disp sound "${longname}.app/Contents/Resources"
	cp bin/main "${longname}.app/Contents/MacOS"
	cp macosx/PkgInfo COPYRIGHT "${longname}.app/Contents"
	if test -e "macosx/${name}.icns"; then cp "macosx/${name}.icns" "${longname}.app/Contents/Resources/app.icns"; else cp macosx/bt.icns "${longname}.app/Contents/Resources/app.icns"; fi
	tar cfz "${name}-${version}-macos.tar.gz" "${longname}.app"

.PHONY: clean
clean:
	rm -rf $(wildcard */*/*.o */*/*.fas */*/*.lib */*/*.fasl */*/*.?x32fsl */*/*.?x64fsl *.db ${tempfile})

.PHONY: docclean
docclean:
	rm -rf doc README.aux README.haux README.htoc README.log README.out README.toc

.PHONY: distclean
distclean:
	$(MAKE) clean docclean
	rm -rf $(wildcard build.in build.out bin)
