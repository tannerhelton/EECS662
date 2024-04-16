test:
	-@raco test interp.rkt > test_results.txt 2>&1
	@echo "Test Results Saved to test_results.txt"
	@echo "Number of Tests Passed:"
	@grep -c "test" test_results.txt | awk '{print $1 " tests passed"}'

run: interp.rkt ast.rkt parser.rkt
	@racket -t main.rkt -m interp.rkt