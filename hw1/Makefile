test:
	-@raco test hw1.rkt > test_results.txt 2>&1
	@echo "Test Results Saved to test_results.txt"
	@echo "Number of Tests Passed:"
	@grep -c "FAILURE" test_results.txt | awk '{print (106 - $$1) " out of 106 tests passed"}'
