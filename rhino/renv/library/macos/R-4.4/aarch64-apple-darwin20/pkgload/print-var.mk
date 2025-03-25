print-%  : ; @echo $($*)

print-compilers:
	@echo $(CC)
	@echo $(CXX)
	@echo $(CXX14)
	@echo $(CXX17)
	@echo $(CXX20)
	@echo $(CXX23)
	@echo $(FC)
	@echo $(OBJC)
