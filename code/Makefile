SUB_DIRECTORIES=primes snowflake

.PHONY: all ${SUB_DIRECTORIES}


all: ${SUB_DIRECTORIES}
	@echo finished
	@echo $<

${SUB_DIRECTORIES}:
	${MAKE} -C $@

