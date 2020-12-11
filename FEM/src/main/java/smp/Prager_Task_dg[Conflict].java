package smp;

import Jama.Matrix;
import Solver.Stiffness;
import feMethods.feMethods;
import Matrix_Algebra.Matrix_Algebra;

public class Prager_Task_dg implements Runnable {

	private feMethods fem = new feMethods();
    private Matrix_Algebra ma = new Matrix_Algebra();
    private Stiffness s = new Stiffness();
    private int o;
    private double[][] r_vec;
    private double V;
    
    private double[] L;
    private double delta;
    private int t_theta;
    private int t_phi;
    private int[] nec_dg;
    
    private int nsd;
    private int nen;
    private int ndf;
    private double[] E;
    private double[] A;
    private double[] I;
    private int[][] ien;
    private int[] flg;
	private double[][] xn;
	private int nel;
	private int neq;
	private int nlc;
	private int[][] id;
	private double[][][] f;
	private double lMin;
	private int nnp;
	private double[][] d_B;
	private double m_a;
	private double[][] dv;
	private double[][] dp;
	private int[] d_g;
	private int idv;
	private int idp;
 
    public Prager_Task_dg(int o, double[] L, double delta, int t_theta, int t_phi, int[] nec_dg,
    		int nsd, int nen, int ndf, double[] E, double[] A, double[] I, int[][] ien,
			int[] flg, double V,double[][] xn,int nel, int neq, int nlc, int[][] id,double[][][] f,int nnp,double[][] d_B, double m_a,
			double[][] dv,double[][] dp, int[] d_g, int idv, int idp) {
		
    	this.o = o;
    	//this.r_vec = r_vec;
    	this.V = V;
    	
    	this.L = L;
        this.delta = delta;
        this.t_theta = t_theta;
        this.t_phi = t_phi;
        this.nec_dg = nec_dg;
        
        
        this.nsd = nsd;
        this.nen = nen;
        this.ndf = ndf;
        this.E = E;
        this.A = A;
        this.I = I;
        this.ien = ien;
        this.flg = flg;
        this.xn = xn;
        this.nel = nel;
        this.neq = neq;
        this.nlc = nlc;
        this.id = id;
        this.f = f;
        this.nnp = nnp;
        this.d_B = d_B;
        this.m_a = m_a;
        this.dp = dp;
        this.dv = dv;
        this.d_g = d_g;
        this.idv = idv;
        this.idp = idp;
	}

	public int getNodeNum() {
        return o;
    }
    
    public double[] getR_Vec_dg(int x) {
        return r_vec[x];
    }
    
    public double getV() {
        return V;
    }
    public double getlMin() {
        return lMin;
    }
 
    public void run() {
        double lMin = fem.minlength(nec_dg, L);
        this.lMin = lMin;
        double[][] r_vec = new double[nnp][nsd];
        this.r_vec = r_vec;
        
        double[] r_test = new double[nsd];
        
        if(idv>-1) { //Vector Contrained Group Case
        	
        	double dvlength = 0;
        	for(int v = 0;v<nsd;v++) {
        		dvlength = dvlength + Math.pow(dv[idv][v+nsd]-dv[idv][v], 2.0);
        	}
        	dvlength = Math.sqrt(dvlength);
        	for(double p = -1.0;p<2;p=p+2){
        		double[][] tmpxn = ma.matrixCopy(xn);
        		for(int v=0;v<nsd;v++) {
        			r_test[v] = (dv[idv][v+nsd]-dv[idv][v])/dvlength*p;
        		}
        		int boundflag = 0;
				for(int n=0;n<nnp;n++) {
					if(d_g[n] == o) {
						for(int e=0;e<nsd;e++) {
							tmpxn[e][n] = tmpxn[e][n]+r_test[e]*delta*lMin;
							if(tmpxn[e][n]<d_B[e][0] || tmpxn[e][n]>d_B[e][1]) {
								boundflag = 1;
							}		
						}
					}
				}
				
				if(boundflag ==0) {
					L = fem.memlength(tmpxn, ien, nel, nsd);
										
					double[][][] Ke = new double[nel][ndf*nen][ndf*nen];
					for(int e = 0;e<nel;e++) {
						Ke[e] = s.Solver(e,nsd,nen,ndf,nel,E,A,I,L,tmpxn,ien,flg);
					}
					
					double[][] K = new double[neq][neq];
					if(neq > 0){
						for(int e = 0;e<nel;e++){
							K = fem.addstiff(K,id,Ke[e],ien[e],nen,ndf);
						}
					}

					Matrix Km = new Matrix(K);
					Matrix Kim = Km.inverse();
					double[][] Ki = Kim.getArray();
					
					double [][] trussaxial = new double[nlc][nel];
					for(int l = 0;l<nlc;l++) {
						double[] F = new double[neq];
						for(int n = 0;n<nnp;n++){
							for(int i = 0; i<ndf;i++){
								if(id[i][n] >= 0){
									int P = id[i][n];
									F[P] = f[l][i][n];
								}
							}
						}
						double[] U = ma.matrixMult(Ki,F);
						trussaxial[l] = fem.trussaxial(nel, nen, nsd, E, A, L, id, ien, tmpxn, U); //Get axial loads in members
					}
					
					double tempVol = 0;
					for(int e = 0;e<nel;e++) {
						//double maxial = Math.abs(ma.matrixMax(ma.matrixColumn(trussaxial, e)));
						tempVol = tempVol + L[e]*Math.max(m_a, Math.abs(ma.matrixAbsMax(ma.matrixColumn(trussaxial, e))));
					}
					
					if(tempVol<=V) {
						V = tempVol;
						for(int n=0;n<nnp;n++) {
							if(d_g[n] == o) {
	    						for(int e=0;e<nsd;e++) {
	    							r_vec[n][e] = r_test[e];
	    						}
							}
						}
					}//Volume check if Statement
				}//boundcheck if statment
        	}
        
        }else if(idp>-1 && nsd ==3) { //Plane Constrained Group Case
        	
        }else { //Free Group Case
        	for(double p = Math.random();p<t_theta;p++){
    			double theta = p*2*Math.PI/t_theta;
    			if(nsd == 2) {
    				t_phi = 1;
    			}
    			for(double q = Math.random();q<t_phi;q++){
    				double phi = q*2*Math.PI/t_phi;
    				
    				//Create Temporary set of nodal coordinates
    				double[][] tmpxn = ma.matrixCopy(xn);
    				if(nsd ==2) {
    					r_test[0] = Math.cos(theta);
    					r_test[1] = Math.sin(theta);
    				}else {
    					r_test[0] = Math.cos(theta);
    					r_test[1] = Math.sin(theta)*Math.cos(phi);
    					r_test[2] = Math.sin(phi)*Math.sin(theta);
    				}
    				//Apply coordinate change, staying within bounds
    				int boundflag = 0;
					for(int n=0;n<nnp;n++) {
						if(d_g[n] == o) {
							for(int e=0;e<nsd;e++) {
								tmpxn[e][n] = tmpxn[e][n]+r_test[e]*delta*lMin;
								if(tmpxn[e][n]<d_B[e][0] || tmpxn[e][n]>d_B[e][1]) {
									boundflag = 1;
								}		
							}
						}
					}
					
    				if(boundflag ==0) {
    					L = fem.memlength(tmpxn, ien, nel, nsd);
    					
    					
    					//Create the array of element stiffness matrices
    					double[][][] Ke = new double[nel][ndf*nen][ndf*nen];
    					for(int e = 0;e<nel;e++) {
    						Ke[e] = s.Solver(e,nsd,nen,ndf,nel,E,A,I,L,tmpxn,ien,flg);
    					}
    					
    					//Compute global K
    					double[][] K = new double[neq][neq];
    					if(neq > 0){
    						for(int e = 0;e<nel;e++){
    							K = fem.addstiff(K,id,Ke[e],ien[e],nen,ndf);
    						}
    					}

    					//Invert
    					Matrix Km = new Matrix(K);
    					Matrix Kim = Km.inverse();
    					double[][] Ki = Kim.getArray();
    					
    					double [][] trussaxial = new double[nlc][nel];
    					
    					for(int l = 0;l<nlc;l++) {
    						
    						//Get Global Force Matrix
    						double[] F = new double[neq];
    						for(int n = 0;n<nnp;n++){
    							for(int i = 0; i<ndf;i++){
    								if(id[i][n] >= 0){
    									int P = id[i][n];
    									F[P] = f[l][i][n];
    								}
    							}
    						}
    						
    						//Get Deformations
    						double[] U = ma.matrixMult(Ki,F);
    						
    						//Axial Forces
    						trussaxial[l] = fem.trussaxial(nel, nen, nsd, E, A, L, id, ien, tmpxn, U); //Get axial loads in members
    					}
    					
    					double tempVol = 0;
    					for(int e = 0;e<nel;e++) {
    						tempVol = tempVol + L[e]*Math.max(m_a, Math.abs(ma.matrixAbsMax(ma.matrixColumn(trussaxial, e))));
    					}
    					
    					if(tempVol<=V) {
    						V = tempVol;
    						for(int n=0;n<nnp;n++) {
    							if(d_g[n] == o) {
		    						for(int e=0;e<nsd;e++) {
		    							r_vec[n][e] = r_test[e];
		    						}
    							}
    						}
    					}//Volume check if Statement
    				}//boundcheck if statment
    			}//phi loop
    		}//theta loop
        }// case if statement
        
		
    }//run loop
}