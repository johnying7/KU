public class Matrix(){
	
	int n;
	double[][] mdata = new[10][10] ;
	
	//initializes the new size of the matrix
	public double Matrix(int _n)
	{
		n = _n;
	}
	
	//finds the determinant of the 2d matrix
	double determinant(){
		double det = 0.0;
		
		if(n == 1)
		{
			det = mdata[0][0];
		}
		else if (n == 2)
		{
			det = mdata[0][0] * mdata[1][1] - mdata[0][1] * mdata[1][0];
		}
		else
		{
			for (int i = 0; i < n; ++i)
			{
				det += pow(-1.0, (double)i) * mdata[0][i] * subMatrix(0, i).determinant();
			}
		}
		
		return det;
	}
	
	void inverse(){
		
	}
	
	void subMatrix(int r, int c)
	{
		
	}
}