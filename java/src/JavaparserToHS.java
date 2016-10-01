import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.PrintWriter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import japa.parser.JavaParser;
import japa.parser.ast.CompilationUnit;

public class JavaparserToHS {
	private static final Logger LOG = LoggerFactory.getLogger(JavaparserToHS.class);

	public static void main(String[] args) throws Exception {
		String infile = args[0];
		String outfile = args[1];

		LOG.info("START");
		try (FileInputStream in = new FileInputStream(infile);
			 PrintWriter out = new PrintWriter(new BufferedWriter(new FileWriter(outfile)))) {
			CompilationUnit cu = JavaParser.parse(in);
			cu.accept(new ToDeriveReadVisitor(out), null);
		} catch (Exception e) {
			LOG.error("Error parsing " + infile, e);
			throw e;
		}
		LOG.info("DONE");
	}
}
