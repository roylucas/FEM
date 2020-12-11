package feMethods;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import javax.swing.JFileChooser;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Row.MissingCellPolicy;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFFormulaEvaluator;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.ejml.data.DMatrixRMaj;
import org.ejml.simple.SimpleMatrix;

import Jama.Matrix;
import Matrix_Algebra.Matrix_Algebra;
import Solver.Stiffness;
import smp.Prager_Task_dg;
import smp.Prager_Task_node;
//import javacl;


public class Main_Prager_3D_smp {

	@SuppressWarnings("deprecation")
	public static void main(String[] args) {
		
		feMethods fem = new feMethods();
		Matrix_Algebra ma = new Matrix_Algebra();
		Stiffness s = new Stiffness();
		//org.ejml.data.Matrix a = new DMatrixRMaj(5,5);
		//SimpleMatrix A = SimpleMatrix.wrap(a);
		//A.invert();
		try {
		
		//File Path input
		JFileChooser chooser = new JFileChooser();
		File start = new File(Main_Prager_3D_smp.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath());

		chooser.setCurrentDirectory(start);
        chooser.showOpenDialog(null);
        String dir = chooser.getSelectedFile().getAbsolutePath();
        File path = new File(dir);

        FileInputStream file = new FileInputStream(path); 
        XSSFWorkbook workbook = new XSSFWorkbook(file);
        workbook.setMissingCellPolicy(Row.CREATE_NULL_AS_BLANK);
        MissingCellPolicy mcp = workbook.getMissingCellPolicy();
        
        int nsd = (int) workbook.getSheet("Parameters").getRow(2).getCell(1).getNumericCellValue(); //Number of spatial dimensions
		int ndf = (int) workbook.getSheet("Parameters").getRow(3).getCell(1).getNumericCellValue(); //Number of degrees of freedom per node, set at 2 from now on for 2D cases since I'm only doing trusses with this I guess
		int nen = 2; //Number of element nodes, as in nodes per element. I think this basically never changes unless I get to plate elements
		//int nnp = 0; //Number of nodal points
		//int nel = 0;
		//Positive X is right, Positive Y is up, Positive Z is out of the page, right-hand rule
		//Compression is negative and Tension is positive
		
        
        int nel = fem.nrExcel(file, workbook, workbook.getSheet("Members"),0); //Get number of elements
        int nbl = fem.nrExcel(file, workbook, workbook.getSheet("Constraints"),0); //Get number of points at which there are boundary conditions
        int nlc = fem.ncExcel(file, workbook, workbook.getSheet("Loads"),0)/(ndf+nsd); //Get number of Load Combinations
        
        double[][] ael = new double[nel][nen*nsd];
        double[][] abl = new double[nbl][nsd+ndf];
        
        ael = fem.readExcel(ael, file, workbook.getSheet("Members"),0,0); //ael[e] = (x1,y1,z1,x2,y2,z2)
        abl = fem.readExcel(abl, file, workbook.getSheet("Constraints"),0,0); //abl[e] = (x,y,bx,by)
        
        double[][] tmp = new double[ael.length*nen][nsd];
        
        for(int e = 0;e<ael.length;e++) {
        	for(int n = 0;n<nen;n++) {
        		for(int x = 0;x<nsd;x++) {
        			tmp[e*nen+n][x] = ael[e][nsd*n+x];
        		}
        	}
		}
        int[] nflg = fem.dupnp(tmp,nsd);
        int nnp = fem.nnp(tmp, nflg, nel, nen, nsd);
        
		//Coordinates
		//xn[i][N] = Coordinate I for global node N
        
        double[][] xn = new double[nsd][nnp];
        int counter = 0;
        for(int e = 0;e<tmp.length;e++) {
        	if(1 != nflg[e]) {
        		for(int n = 0;n<nsd;n++) {
        			xn[n][counter] = tmp[e][n];
        		}
        		counter++;
    		}
        }
        
        //Connectivity
		//ien[e][a] = N
		//a = local node number. Like i and j ends of an element
		//e = Global element number
		//N = Global node number
        
        int[][] ien = new int[nel][nen];
        for(int e = 0;e<nel;e++) {
			for(int p = 0;p<nnp;p++) {
				for(int n = 0;n<nen;n++) {
					int connect = 0;
					for(int v = 0;v<nsd;v++) {
						if(ael[e][n*nsd+v] == xn[v][p]) {
							connect = connect + 1;
						}
					}
					if(connect == nsd) {
						ien[e][n] = p;
					}
				}
				/*if(ael[e][0] == xn[0][p] & ael[e][1] == xn[1][p]) {
					ien[e][0] = p;
				}
				if(ael[e][2] == xn[0][p] & ael[e][3] == xn[1][p]) {
					ien[e][1] = p;
				}*/
			}
        }
        
        
        
        double[][] prop = new double[nel][4];
        prop = fem.readExcel(prop, file, workbook.getSheet("Properties"),0,0); //prop[e] = (A,E,TC,I)
        
        //Initialize Area Matrix
		double[] A = new double[nel];
		
		//Initialize Young's Elastic Modulus Matrix
		double[] E = new double[nel];
		
		//Initialize Tension and Compression Flag Matrix
		//tc[e] = 0, 1, or -1
		//e = member being flagged. 0 = T&C, 1 = Tension only, -1 = Compression only
		int[] tc = new int[nel];
		
		//Initialize Second Moment of Area (Inertia) Matrix
		double[] I = new double[nel];
		
		for(int e = 0; e<nel;e++) {
			if(prop[e][0] == 0) {
				A[e] = 1;
			}else {
				A[e] = prop[e][0];
			}
			
			if(prop[e][1] == 0) {
				E[e] = 1;
			}else {
				E[e] = prop[e][1];
			}
			
			tc[e] = (int) prop[e][2];
			
			if(prop[e][3] == 0) {
				I[e] = 1;
			}else {
				I[e] = prop[e][3];
			}		
		}
		
		//Lengths
		//L[0] = length of global element 1
		double[] L = new double[nel];
		L = fem.memlength(xn, ien, nel, nsd);
		
		//Deformation Scale Factor
		double scalefactor = workbook.getSheet("Parameters").getRow(0).getCell(1).getNumericCellValue();
		
		//Beam Resolution, determines number of internal points per beam element
		int beamRes = (int) workbook.getSheet("Parameters").getRow(1).getCell(1).getNumericCellValue();
		
		//Boundary Conditions
		//idb[i][N] = 1
		//i = dof being fixed. X = 0, Y = 1, Z = 2, X-ROT = 3, Y-ROT - 4, Z-ROT = 5
		//N = Global node number being perscribed
		
		int[][] idb = new int[ndf][nnp];
		
		for(int e = 0;e<nbl;e++) {
			for(int n = 0;n<nnp;n++) {
				int bound = 0;
				for(int v = 0;v<nsd;v++) {
					if(abl[e][v] == xn[v][n]) {
						bound = bound+1;
					}
				}
				if(bound==nsd) {
					for(int v = 0;v<nsd;v++) {
						idb[v][n] = Math.max((int) abl[e][v+nsd],idb[v][n]);
					}
				}
			}
		}
		
		
		//Design Vectors, Planes and Groups
		
		
		
		int ndg = fem.ncExcel(file, workbook, workbook.getSheet("Design Groups"),0)/nsd; //Get number of Design Groups
		
		int[] idv_node = new int[nnp]; //idv_node[Global Node Number] = Global Design Vector Number
		int[] idp_node = new int[nnp]; //idp_node[Global Node Number] = Global Design Plane Number
		int[] d_g = new int[nnp];
		
		for(int n=0;n<nnp;n++) {
			idv_node[n] = -1;
			idp_node[n] = -1;
			d_g[n] = -1;
		}
		
		if(ndg>0) {
			for(int n=0;n<ndg;n++) {
				int ndg_n = fem.nrExcel(file, workbook, workbook.getSheet("Design Groups"),n*nsd); //Get number of points at which there are forces
				double[][] adg = new double[ndg_n][nsd];
		        adg = fem.readExcel(adg, file, workbook.getSheet("Design Groups"),0,n*nsd); //afl[e] = (x,y,fx,fy)
		        for(int e = 0;e<ndg_n;e++) {
		        	
					for(int o = 0;o<nnp;o++) {
						int group = 0;
						for(int v = 0;v<nsd;v++) {
							if(adg[e][v] == xn[v][o]) {
								group = group+1;
							}
						}
						if(group==nsd) {
							d_g[o] = n;
						}
					}
					
				}
			}
		}
		
		
		int[] idv_dg = new int[ndg]; //idv_dg[Global DG Number] = Global Design Vector Number
		int[] idp_dg = new int[ndg]; //idp_dg[Global DG Number] = Global Design Plane Number
		
		for(int n=0;n<ndg;n++) {
			idv_dg[n] = -1;
			idp_dg[n] = -1;
		}
		
		int ndv = fem.nrExcel(file, workbook, workbook.getSheet("Design Vectors"),0); //Get number of Design Vectors
		double[][] adv = new double[ndv][nsd*nen]; //adv[Global Design Vector Number][Spatial Dim + NSD*Local Node Number] = coordinate
		
		if(ndv >0) {
			adv = fem.readExcel(adv, file, workbook.getSheet("Design Vectors"),0,0);
			for(int n=0;n<ndv;n++) {
				int no_node_flag=0;
				for(int o = 0;o<nnp;o++) {
					for(int i=0;i<nen;i++) {
						int vector = 0;
						for(int v = 0;v<nsd;v++) {
							if(adv[n][v+i*nsd] == xn[v][o]) {
								vector = vector+1;
							}
						}
						if(vector==nsd) {
							idv_node[o] = n;
							no_node_flag=1;
						}
					}	
				}
				if(no_node_flag==0) {
					for(int g=0;g<ndg;g++) {
						int ndg_n = fem.nrExcel(file, workbook, workbook.getSheet("Design Groups"),g*nsd); //Get number of points at which there are forces
						double[][] adg = new double[ndg_n][nsd];
				        adg = fem.readExcel(adg, file, workbook.getSheet("Design Groups"),0,g*nsd); //afl[e] = (x,y,fx,fy)
				        for(int e = 0;e<ndg_n;e++) {
							int group = 0;
							for(int v = 0;v<nsd;v++) {
								if(adg[e][v] == adv[n][v]) {
									group = group+1;
								}
							}
							if(group==nsd) {
								idv_dg[g] = n;
							}
						}
					}
				}
			}
		}
		
		
		int ndp = fem.nrExcel(file, workbook, workbook.getSheet("Design Planes"),0); //Get number of Design Planes
		double[][] adp = new double[ndp][nsd*3];
		if(ndp > 0) {
			adp = fem.readExcel(adp, file, workbook.getSheet("Design Planes"),0,0);
			for(int n=0;n<ndp;n++) {
				int no_node_flag=0;
				for(int o = 0;o<nnp;o++) {
					for(int i=0;i<3;i++) {
						int plane = 0;
						for(int v = 0;v<nsd;v++) {
							if(adp[n][v+i*nsd] == xn[v][o]) {
								plane = plane+1;
							}
						}
						if(plane==nsd) {
							idp_node[o] = n;
							no_node_flag=1;
						}
					}	
				}
				if(no_node_flag==0) {
					for(int g=0;g<ndg;g++) {
						int ndg_n = fem.nrExcel(file, workbook, workbook.getSheet("Design Groups"),g*nsd); //Get number of groups
						double[][] adg = new double[ndg_n][nsd];
				        adg = fem.readExcel(adg, file, workbook.getSheet("Design Groups"),0,g*nsd); //afl[e] = (x,y,fx,fy)
				        for(int e = 0;e<ndg_n;e++) {
							int group = 0;
							for(int v = 0;v<nsd;v++) {
								if(adg[e][v] == adp[n][v]) {
									group = group+1;
								}
							}
							if(group==nsd) {
								idp_dg[g] = n;
							}
						}
					}
				}
			}
		}
		
		
		//Short-Hand Nodal Connectivity, for determining minimum length member connected to a particular node without too much math in execution
        //nec[n][e] = flag
        //n = Global Node Number
        //e = Global Element Number
        //flag = 1 for connected or 0 for not
        int[][] nec = new int[nnp][nel];
        for(int e=0;e<nel;e++) {
        	for(int n=0;n<nen;n++) {
        		nec[ien[e][n]][e] = 1;	
        	}      	
        }
        
        int[][] nec_dg = new int[ndg][nel];
        for(int p=0;p<ndg;p++) {
        	for(int o=0;o<nnp;o++) {
        		if(d_g[o] == p) {
        			for(int e=0;e<nel;e++) {
        	        	for(int n=0;n<nen;n++) {
        	        		if(ien[e][n] == o) {
        	        			nec_dg[p][e] = 1;
        	        		}
        	        	}
        			}
        		}
        	}
        }
				
		//Member Type Flags, as of 171217 it doesn't work and I have no idea why I can't use the same size matrix for both a truss and a beam
		// As of 281218 Still doesn't work but I'm working on being able to plot beam elements so 
		//flg[e] = 0 or 1
		//e = member being flagged. 0 = Truss, 1 = Beam
				
		int[] flg = new int[nel];
				
		
		//Number of equations and ID table
		ID identity = new ID(nnp,ndf, idb);
		int[][] id = identity.getIDtable();
		int neq = identity.getneq();
		
		
		//Prescribed Nodal Forces
		//f[Degree of Freedom][Global Node Number] = Prescribed Force
			
		double[][][] f = new double[nlc][ndf][nnp];
		for(int l = 0;l<nlc;l++) {
			int nfl = fem.nrExcel(file, workbook, workbook.getSheet("Loads"),l*(nsd+ndf)); //Get number of points at which there are forces
			double[][] afl = new double[nfl][nsd+ndf];
	        afl = fem.readExcel(afl, file, workbook.getSheet("Loads"),0,l*(nsd+ndf)); //afl[e] = (x,y,fx,fy)
	        for(int e = 0;e<nfl;e++) {
				for(int n = 0;n<nnp;n++) {
					int force = 0;
					for(int v = 0;v<nsd;v++) {
						if(afl[e][v] == xn[v][n]) {
							force = force+1;
						}
					}
					if(force==nsd) {
						for(int v = 0;v<nsd;v++) {
							f[l][v][n] = f[l][v][n] + afl[e][v+nsd];
						}
					}
				}
			}
		}
		
		
		//Minimum Axial load, used for volume computation only. Provides a little more stability to the design. Can kind of simulate a minimum member size
		double min_axial = workbook.getSheet("Parameters").getRow(9).getCell(1).getNumericCellValue();
		
		//Percentage to member iteration. 0.0 being no iteration and 0.5 being a straight average. Larger values weight new axial load more
		double mempercent = workbook.getSheet("Parameters").getRow(10).getCell(1).getNumericCellValue();
		
		
		//Initialize Axial force Matrix
		double [][] trussaxial = new double[nlc][nel];
		
		//Run through every combo once and get an initial volume
		for(int l = 0;l<nlc;l++) {
			
			//input prescribed nodal forces in F
			double[] F = new double[neq];
			for(int n = 0;n<nnp;n++){
				for(int i = 0; i<ndf;i++){
					if(id[i][n] >= 0){
						int P = id[i][n];
						F[P] = f[l][i][n];
					}
				}
			}
			
			//Create the array of element stiffness matrices
			double[][][] Ke = new double[nel][ndf*nen][ndf*nen];
			for(int e = 0;e<nel;e++) {
				Ke[e] = s.Solver(e,nsd,nen,ndf,nel,E,A,I,L,xn,ien,flg);
			}
			
			//Compute global K
			double[][] K = new double[neq][neq];
			if(neq > 0){
				for(int e = 0;e<nel;e++){
					K = fem.addstiff(K,id,Ke[e],ien[e],nen,ndf);
				}
			}

			//Invert and Solve
			//double[][] Ki = fem.EJMLsolve(K);
			double[][] Ki = fem.JAMAsolve(K);
			double[] U = new double[neq];
			U = ma.matrixMult(Ki,F);
			
			
			//Axial Forces
			trussaxial[l] = fem.trussaxial(nel, nen, nsd, E, A, L, id, ien, xn, U); //Get axial loads in members
		}
		
		
		
		
		//Create a value for "volume" of truss members. Axial load times length is an appropriate analog
		double Vmin = 0;
		for(int n = 0;n<nel;n++) {
			Vmin = Vmin + L[n]*Math.max(min_axial, Math.abs(ma.matrixAbsMax(ma.matrixColumn(trussaxial, n))));
		}
		double Vstart = Vmin;
		
		//Scale Member Areas based on initial axial load
		for(int n = 0;n<nel;n++) {
			A[n] = (A[n]*(2-2*mempercent)+2*mempercent*Math.max(min_axial, Math.abs(ma.matrixAbsMax(ma.matrixColumn(trussaxial, n)))))/2;
		}
		
		//Initialize an array telling whether each node can be moved. Count the number of moveable nodes. Counting the number of moveables is not used for anything as of 2020-08-19
		//Could pin nodes so they only move parallel to an applied load or something
		int[] moveable = new int[nnp];
		int nummove = nnp;
		
		for(int n = 0;n<nnp;n++) {
			for(int v = 0;v<ndf;v++) {
				for(int l = 0;l<nlc;l++) {
					if(idb[v][n]>0 || Math.abs(f[l][v][n])>0) {
						moveable[n] = 1; 
					}
				}
			}
			nummove = nummove - moveable[n];
		}
		
		//Constants for the Design Iteration Process
		int iteration = (int) workbook.getSheet("Parameters").getRow(4).getCell(1).getNumericCellValue();
		int t_theta = (int) workbook.getSheet("Parameters").getRow(5).getCell(1).getNumericCellValue();
		int t_phi = (int) workbook.getSheet("Parameters").getRow(6).getCell(1).getNumericCellValue();
		double delta = workbook.getSheet("Parameters").getRow(7).getCell(1).getNumericCellValue();
		double full_drift = workbook.getSheet("Parameters").getRow(8).getCell(1).getNumericCellValue();
		
		double[][] design_Bounds = new double[nsd][2];
		design_Bounds = fem.readExcel(design_Bounds, file, workbook.getSheet("Design Bounds"),1,1);
		
		for(int n=0;n<nsd;n++) {
			if(design_Bounds[n][0] == design_Bounds[n][1] ) {
				design_Bounds[n][0] = -9999999999999999.0;
				design_Bounds[n][1] = 9999999999999999.0;
			}
		}
		
		//More is better, up to a point
		int threadCount = Math.max(Math.min(nummove, 7),1);
		
		double[] gradient = new double[iteration+1];
		gradient[0] = Vstart;
		
		Prager_Task_node[] p_tarray = new Prager_Task_node[nnp];
		Prager_Task_dg[] p_tarray_dg = new Prager_Task_dg[ndg];
		
		double cycleStartTime = System.currentTimeMillis();
		double[][] U = new double[nlc][neq];
		
		double[][][] topo = new double[iteration+1][nsd][nnp];
		topo[0] = ma.matrixCopy(xn);
		// double[][] xn = new double[nsd][nnp];
		
		
		//Main iteration Loop //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		for(int n = 0;n<iteration;n++) {
			double iterationStartTime = System.currentTimeMillis();
			
			double drift = full_drift/Math.log(n+2);
			double[] Varray = new Matrix(nnp,1,Vmin).getColumnPackedCopy();
			double[][] r_vec = new double[nnp][nsd];
			L = fem.memlength(xn, ien, nel, nsd);
			double[] lMin = new double[nnp];
			
			
			ThreadPoolExecutor executor = (ThreadPoolExecutor) Executors.newFixedThreadPool(threadCount);
			
			
			for(int o=0;o<nnp;o++) {
				if(moveable[o]==0) {
					if(d_g[o] ==-1 || d_g[o]>-1 && idv_node[o]>-1 || d_g[o]>-1 && idp_node[o]>-1 ) {
						p_tarray[o] = new Prager_Task_node(o,ma.matrixCopy(L),delta,t_theta,t_phi,nec[o],nsd,nen,ndf,E,A,I,ien,flg,Vmin,xn,nel,neq,nlc,id,f,nnp,
								design_Bounds,min_axial,adv,adp,idv_node[o],idp_node[o],drift);
						executor.execute(p_tarray[o]);
					}
				}
			}
			for(int o=0;o<ndg;o++) {
				p_tarray_dg[o] = new Prager_Task_dg(o,ma.matrixCopy(L),delta,t_theta,t_phi,nec_dg[o],nsd,nen,ndf,E,A,I,ien,flg,Vmin,xn,nel,neq,nlc,id,f,nnp,
														design_Bounds,min_axial,adv,adp,d_g,idp_dg[o],idp_dg[o],drift);
				executor.execute(p_tarray_dg[o]);
			}
			
			executor.shutdown();
			executor.awaitTermination(1L, TimeUnit.HOURS);
			
			for(int o=0;o<ndg;o++) {
	        	for(int p=0;p<nnp;p++) {
	        		if(d_g[p] == o) {
	        			lMin[p] = p_tarray_dg[o].getlMin();
	        			r_vec[p] = p_tarray_dg[o].getR_Vec_dg(p);
	        			for(int e = 0;e<nsd;e++) {
	        				r_vec[p][e] = r_vec[p][e] + r_vec[p][e]*lMin[p]*drift;
	        			}
	        		}
	        	}
	        }
			
	        for(int o = 0; o<nnp;o++) {
	        	if(moveable[o]==0) {
	        		if(d_g[o] ==-1 || d_g[o]>-1 && idv_node[o]>-1 || d_g[o]>-1 && idp_node[o]>-1 ) {
		        		lMin[o] = p_tarray[o].getlMin();
		        		double[] r_temp = p_tarray[o].getR_Vec();
		        		for(int e = 0;e<nsd;e++) {
		        			r_vec[o][e] = r_vec[o][e] + r_temp[e]*lMin[o]*drift;
		        		}
	        		}
	        	}
	        }
	        
	        int test = 0;
	        
	        for(int o = 0;o<nnp;o++) {
	        	if(moveable[o]==0) {
	        		for(int e = 0;e<nsd;e++) {
	        			xn[e][o] = xn[e][o] + r_vec[o][e];
	        		}
	        	}
	        }
	        
	        topo[n+1] = ma.matrixCopy(xn);
	        
			L = fem.memlength(xn, ien, nel, nsd);
			
			for(int l = 0;l<nlc;l++) {
				double[] F = new double[neq];
				
				for(int o = 0;o<nnp;o++){
					for(int i = 0; i<ndf;i++){
						if(id[i][o] >= 0){
							int P = id[i][o];
							F[P] = f[l][i][o];
						}
					}
				}
				
				//Create the array of element stiffness matrices
				double[][][] Ke = new double[nel][ndf*nen][ndf*nen];
				for(int e = 0;e<nel;e++) {
					Ke[e] = s.Solver(e,nsd,nen,ndf,nel,E,A,I,L,xn,ien,flg);
				}
				
				//Compute global K
				double[][] K = new double[neq][neq];
				if(neq > 0){
					for(int e = 0;e<nel;e++){
						K = fem.addstiff(K,id,Ke[e],ien[e],nen,ndf);
					}
				}

				//Invert and Solve
				//double[][] Ki = fem.EJMLsolve(K);
				double[][] Ki = fem.JAMAsolve(K);
				U[l] = ma.matrixMult(Ki,F);
				
				//Axial Forces
				trussaxial[l] = fem.trussaxial(nel, nen, nsd, E, A, L, id, ien, xn, U[l]); //Get axial loads in members
				
				//Need some way to iterate the stiffness of members based on axial load
				
			}//Load Combo Loop
			
			//Objective Function recalculation
			Vmin = 0;
			for(int e = 0;e<nel;e++) {
				Vmin = Vmin + L[e]*Math.max(min_axial, Math.abs(ma.matrixAbsMax(ma.matrixColumn(trussaxial, e))));
			}
			
			//Member stiffness iteration
			for(int e=0;e<nel;e++) {
				A[e] = (2*(1-mempercent)*A[e]+2*mempercent*Math.max(min_axial, Math.abs(ma.matrixAbsMax(ma.matrixColumn(trussaxial, e)))))/2;
			}
			
			gradient[n+1] = Vmin;
			double iterationEndTime = System.currentTimeMillis();
			System.out.println("#"+(n+1)+" - "+Vmin+ "  -  " + (iterationEndTime - iterationStartTime) +" ms taken");
			
		}//Main Iteration Loop
		
		//Print Total Design Cycle Time Taken
		double cycleEndTime = System.currentTimeMillis();
		System.out.println();
		System.out.println((cycleEndTime - cycleStartTime)+" ms taken in total");
		System.out.println();
		//Print deformations and axial loads just because
		ma.matrixPrint(U);
		ma.matrixPrint(trussaxial);
		System.out.println(Vstart);
		System.out.println(Vmin);
		//ma.matrixPrint(axial);
		
		for(int e = 0;e<iteration+1;e++) {
			workbook.getSheet("Gradient").createRow(e);
			workbook.getSheet("Gradient").getRow(e).getCell(0).setCellValue(e);
			workbook.getSheet("Gradient").getRow(e).getCell(1).setCellValue(gradient[e]);
		}
		
		for(int e = 0;e<nel;e++) {
			workbook.getSheet("Topology").createRow(e);
			workbook.getSheet("CrossSection").createRow(e);
			for(int n = 0;n<nen;n++) {
				for(int x = 0;x<nsd;x++) {
					for(int i=0;i<iteration+1;i++) {
						workbook.getSheet("Topology").getRow(e).getCell(n*nsd+x+i*nen*nsd).setCellValue(((double) Math.round(topo[i][x][ien[e][n]]*10000.0))/10000.0);
					}
					//workbook.getSheet("Topology").getRow(e).getCell(n*nsd+x).setCellValue(((double) Math.round(xn[x][ien[e][n]]*10000.0))/10000.0);
				}
			}
			workbook.getSheet("CrossSection").getRow(e).getCell(0).setCellValue(((double) Math.round(A[e]*1000000.0))/1000000.0);
		}
		
		for(int l = 0;l<nlc;l++) {
			if(nsd == ndf) { //Truss Case
				double[][][] coord = fem.trussdeform(nel,nen,nsd,E,A,L,id,ien,xn,U[l]);
				for(int e = 0;e<nel;e++) {
					if(l==0) {
						//workbook.getSheet("Topology").createRow(e);
						workbook.getSheet("Deformation").createRow(e);
						workbook.getSheet("Axial").createRow(e);
					}
					for(int n = 0;n<nen;n++) {
						for(int x = 0;x<nsd;x++) {
							workbook.getSheet("Deformation").getRow(e).getCell(n*nsd+x+l*nen*nsd).setCellValue(((double) Math.round(coord[e][n][x]*10000.0))/10000.0);
						}
					}
					workbook.getSheet("Axial").getRow(e).getCell(l).setCellValue(trussaxial[l][e]);
				}
				
			} else if(nsd == 2 && ndf == 3){ //2D Beam/Frame Case
				double[][][] coord = fem.beamdeform(nel,nen,nsd,E,A,I,L,id,ien,xn,U[l],scalefactor,beamRes);
				for(int e = 0;e<nel;e++) {
					workbook.setMissingCellPolicy(mcp);
					for(int n = 0;n<nen+beamRes;n++) {
						for(int x = 0;x<nsd;x++) {
							XSSFRow newrow = workbook.getSheet("Deformation").getRow(e);
							if(newrow == null) {
								newrow = workbook.getSheet("Deformation").createRow(e);
							}
							XSSFCell newcell = newrow.getCell(n*nsd+x+l*nen*nsd, mcp);
							newcell.setCellValue(coord[e][n][x]);
						}
					}
					
					XSSFRow newrow = workbook.getSheet("Moment").getRow(e);
					if(newrow == null) {
						newrow = workbook.getSheet("Moment").createRow(e);
					}
					for(int n = 0;n<nen;n++) {
						XSSFCell newcell = newrow.getCell(n, mcp);
						newcell.setCellValue(fem.moment[e][n]);
					}
					
					newrow = workbook.getSheet("Axial").getRow(e);
					if(newrow == null) {
						newrow = workbook.getSheet("Axial").createRow(e);
					}
					XSSFCell newcell = newrow.getCell(0, mcp);
					newcell.setCellValue(fem.axial[e]);
					
					//workbook.getSheet("Results").getRow(e).getCell(4+l*5, Row.CREATE_NULL_AS_BLANK).setCellValue(axial[e]);
				}
			}else { //Other Case, program does not support it yet
				for(int e = 0;e<nel;e++) {
					for(int n = 0;n<nen;n++) {
						for(int x = 0;x<nsd;x++) {
							workbook.getSheet("Results").getRow(e).getCell(n*nen+x+l*5).setCellValue("Error");
						}
					}
					workbook.getSheet("Results").getRow(e).getCell(4+l*5).setCellValue("Error");
				}
			}
			//FileOutputStream outputStream = new FileOutputStream(path);
	        //workbook.write(outputStream);
		}
		//}
        file.close();
		FileOutputStream outputStream = new FileOutputStream(path);
        workbook.write(outputStream);
        //XSSFFormulaEvaluator.evaluateAllFormulaCells(workbook);
        workbook.close();
        
		} catch (IOException t) {
	         t.printStackTrace();
	    } catch (Exception t) {
	         t.printStackTrace();
	    }
	} 
}
