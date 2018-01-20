import utest.ui.Report;
import utest.Runner;

class Test {
	static public function main() {
		var runner = new Runner();
		runner.addCases('cases');
		Report.create(runner);
		runner.run();
	}
}