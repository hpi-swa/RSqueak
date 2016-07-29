baseline
baseline: spec
	<baseline>
	spec
		for: #'common'
		do: [
			spec
				package: 'BenchmarkRunner' with: 
					[ spec
							file: 'Benchmark-tfel.22';
							repository: 'http://www.hpi.uni-potsdam.de/hirschfeld/squeaksource/BenchmarkRunner' ];
				package: 'SQPyte' with: [spec requires: #('BenchmarkRunner')];
				yourself.
			spec
				group: 'default' with: #('SQPyte') ];
				yourself