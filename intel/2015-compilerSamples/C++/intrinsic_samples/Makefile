intel:  intrin_dot_sample.c intrin_double_sample.c intrin_ftz_sample.c
	icc -o dot_sample intrin_dot_sample.c
	icc -o double_sample intrin_double_sample.c
	icc -o ftz_sample intrin_ftz_sample.c

clang:  intrin_dot_sample.c intrin_double_sample.c intrin_ftz_sample.c
	icl -o dot_sample intrin_dot_sample.c
	icl -o double_sample intrin_double_sample.c
	icl -o ftz_sample intrin_ftz_sample.c

gcc:  intrin_dot_sample.c intrin_double_sample.c intrin_ftz_sample.c
	gcc -msse3 -o dot_sample intrin_dot_sample.c
	gcc -msse3 -o double_sample intrin_double_sample.c
	gcc -msse3 -o ftz_sample intrin_ftz_sample.c
    
clean:
	@echo " Cleaning..."
	@rm dot_sample double_sample ftz_sample

run:
	@echo " Running Dot Sample..."
	@./dot_sample
	@echo " Running Double Sample..."
	@./double_sample
	@echo " Running FTZ Sample..."
	@./ftz_sample
