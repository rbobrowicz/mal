
STEPS = step0_repl step1_read_print step2_eval

all:
	stack build

$(STEPS):
	stack build :$@

clean:
	stack clean
