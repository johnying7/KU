package testing;
import static org.junit.Assert.*;

import org.junit.Test;

public class MatrixTest {

	int n;
	double[][] mdata = new double[3][3] ;
	
	@Test
	public void testMatrix() {
		fail("Not yet implemented");
	}

	@Test
	public void testDeterminant() {
		
		double[][] matrix = new double[3][3];
		
		matrix[0][0] = 5;
		matrix[0][1] = 4;
		matrix[0][2] = 3;
		matrix[1][0] = 6;
		matrix[1][1] = 9;
		matrix[1][2] = 1;
		matrix[2][0] = 2;
		matrix[2][1] = 5;
		matrix[2][2] = 3;
		
		assertEquals(82.0, testing.Matrix.determinant(3,matrix),0.001);
		//fail("Not yet implemented");
	}

	@Test
	public void testInverse() {
		
		//fail("Not yet implemented");
	}

	@Test
	public void testSubMatrix() {
		double[][] matrix = new double[3][3];
		
		matrix[0][0] = 5;
		matrix[0][1] = 4;
		matrix[0][2] = 3;
		matrix[1][0] = 6;
		matrix[1][1] = 9;
		matrix[1][2] = 1;
		matrix[2][0] = 2;
		matrix[2][1] = 5;
		matrix[2][2] = 3;
		
		assertEquals(82.0, testing.Matrix.determinant(3,matrix),0.001);
		
		//fail("Not yet implemented");
	}

}
