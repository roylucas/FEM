package Solver;
import Jama.Matrix;
import Matrix_Algebra.Matrix_Algebra;

public class Beam {
	
	public double[][] Ke_Beam(double[][] Ke, int nel, double E, double A, double I, double L, double[][] xn, int[] ien, int ndf, int nen){

			
			Matrix_Algebra ma = new Matrix_Algebra();
			
			double[][] ke = Two_D_Beam_Local_Stiff(E, A, I, L, nen, ndf).getArray();
			
			Matrix Qe = new Matrix(ndf*nen,ndf*nen);
			int n1 = ien[0];  //Returns global number of i node of element "e"
			int n2 = ien[1];  //Returns global number of j node of element "e"
			Qe = Two_D_Beam_Rotation_Matrix(xn, ien, L, n1, n2, nen, ndf);
			
			
			double[][] QeI = new double[ndf*nen][ndf*nen];
			QeI = Qe.inverse().getArray();
			
			Ke = ma.matrixMult(ma.matrixMult(QeI, ke), Qe.getArray());
			
		return Ke;
	}
	
	Matrix Two_D_Beam_Local_Stiff(double E, double A, double I, double L, int nen, int ndf){
		Matrix ke = new Matrix(nen*ndf,nen*ndf);
		ke.set(0, 0, E*A/L);
		ke.set(0, 3, -E*A/L);
		ke.set(1,1,12*E*I/Math.pow(L,3.0));
		ke.set(1, 2, 6*E*I/Math.pow(L,2.0));
		ke.set(1, 4, -12*E*I/Math.pow(L,3.0));
		ke.set(1, 5, 6*E*I/Math.pow(L,2.0));
		ke.set(2, 1, 6*E*I/Math.pow(L,2.0));
		ke.set(2, 2, 4*E*I/L);
		ke.set(2, 4, -6*E*I/Math.pow(L,2.0));
		ke.set(2, 5, 2*E*I/L);
		ke.set(3, 0, -E*A/L);
		ke.set(3, 3, E*A/L);
		ke.set(4, 1, -12*E*I/Math.pow(L,3.0));
		ke.set(4, 2, -6*E*I/Math.pow(L,2.0));
		ke.set(4, 4, 12*E*I/Math.pow(L,3.0));
		ke.set(4, 5, -6*E*I/Math.pow(L,2.0));
		ke.set(5, 1, 6*E*I/Math.pow(L,2.0));
		ke.set(5, 2, 2*E*I/L);
		ke.set(5, 4, -6*E*I/Math.pow(L,2.0));
		ke.set(5, 5, 4*E*I/L);
		
		return ke;
	}
	
	Matrix Two_D_Beam_Rotation_Matrix(double[][] xn, int[] ien, double L, int n1, int n2, int nen, int ndf){
		Matrix Qe = new Matrix(nen*ndf, nen*ndf);
		double[] v = new double[2];
		v[0] = (xn[0][n2] - xn[0][n1])/L;
		v[1] = (xn[1][n2] - xn[1][n1])/L;
		Qe.set(0, 0, v[0]);
        Qe.set(0, 1, v[1]);
        Qe.set(1,0,-v[1]);
        Qe.set(1, 1, v[0]);
        Qe.set(2, 2, 1);
        Qe.set(3, 3, v[0]);
        Qe.set(3, 4, v[1]);
        Qe.set(4, 3, -v[1]);
        Qe.set(4, 4, v[0]);
        Qe.set(5, 5, 1);
	
		return Qe;
	}
	
	
}
