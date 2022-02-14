FC = gfortran           # for gfortran
LD = gfortran           # for gfortran
RM = rm -f              # for Linux
#RM = del /F            # for Windows

.SUFFIXES:  .f90 .f77

SOURCES = compiler_specific.f90 \
          parser_definitions.f90 \
          convert_int_character.f90 \
          parser_modules.f90 \
          fparser.f90 \
          syntax_database.f90 syntax_inputfile.f90 \
          parser.f90 \
          builder_database.f90 builder_inputfile.f90 \
          parser_read_in_keywords_database.f90 parser_read_in_keywords_inputfile.f90 \
          parser_input_driver.f90 \
          FortranInputParser.f90

OBJECTS1 = $(SOURCES:.f90=.o)
OBJECTS = $(OBJECTS1:.f=.o)

.mod.o: ;

.f.o:
	$(FC) -c $(FFLAGS) $<

.f90.o:
	$(FC) -c $(FFLAGS) $<

linux: $(OBJECTS)
	$(LD) -o FortranInputParser.exe $(LDFLAGS) $(OBJECTS)

windows: $(OBJECTS)
	$(LD) -o FortranInputParser.exe $(LDFLAGS) *.obj

$(SOURCES):

clean: 
	$(RM) *.o  *~  *.mod  core  *"#"  *.obj  *.stb  FortranInputParser.exe
