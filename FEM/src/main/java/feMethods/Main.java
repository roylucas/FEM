package feMethods;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import javax.swing.JFileChooser;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Row.MissingCellPolicy;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFFormulaEvaluator;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

import Jama.Matrix;
import Matrix_Algebra.Matrix_Algebra;
import Solver.Stiffness;



public class Main {

	@SuppressWarnings("deprecation")
	public static void main(String[] args) {
		
		feMethods fem = new feMethods();
		Matrix_Algebra ma = new Matrix_Algebra();
		Stiffness s = new Stiffness();
		int nsd = 2; //Number of spatial dimensions
		int ndf = 3; //Number of degrees of freedom per node, set at 2 from now on for 2D cases since I'm only doing trusses with this I guess
		int nen = 2; //Number of element nodes, as in nodes per element. I think this basically never changes unless I get to plate elements
		//int nnp = 0; //Number of nodal points
		//int nel = 0;
		//Positive X is right, Positive Y is up, Positive Z is out of the page, right-hand rule
		//Compression is negative and Tension is positive
		
		try {
		
		//File Path input
		JFileChooser chooser = new JFileChooser();
		File start = new File(Main.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath());

		chooser.setCurrentDirectory(start);
        chooser.showOpenDialog(null);
        String dir = chooser.getSelectedFile().getAbsolutePath();
        File path = new File(dir);

        FileInputStream file = new FileInputStream(path); 
        XSSFWorkbook workbook = new XSSFWorkbook(file);
        workbook.setMissingCellPolicy(Row.CREATE_NULL_AS_BLANK);
        MissingCellPolicy mcp = workbook.getMissingCellPolicy();
        
        int nel = fem.nrExcel(file, workbook, workbook.getSheet("Members"),0); //Get number of elements
        int nbl = fem.nrExcel(file, workbook, workbook.getSheet("Constraints"),0); //Get number of points at which there are boundary conditions
        int nlc = fem.ncExcel(file, workbook, workbook.getSheet("Loads"),0)/5; //Get number of Load Combinations
        
        double[][] ael = new double[nel][nen*nsd];
        double[][] abl = new double[nbl][nsd+ndf];
        
    	ael = fem.readExcel(ael, file, workbook.getSheet("Members"),0,0); //ael[e] = (x1,y1,z1,x2,y2,z2)
        abl = fem.readExcel(abl, file, workbook.getSheet("Constraints"),0,0); //abl[e] = (x,y,bx,by)
        
        double[][] tmp = new double[ael.length*nen][nsd];
        
        for(int e = 0;e<ael.length;e++) {
        	for(int n = 0;n<nen;n++) {
        		for(int x = 0;x<nsd;x++) {
        			tmp[e*nen+n][x] = ael[e][nen*n+x];
        		}
        	}
		}
        int[] nflg = fem.dupnp(tmp, nsd);
        int nnp = fem.nnp(tmp, nflg, nel, nen, nsd);
        
		//Coordinates
		//xn[i][N] = Coordinate I for global node N
        
        double[][] xn = new double[nsd][nnp];
        int counter = 0;
        for(int e = 0;e<tmp.length;e++) {
        	if(1 != nflg[e]) {
        		xn[0][counter] = tmp[e][0];
        		xn[1][counter] = tmp[e][1];
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
				if(ael[e][0] == xn[0][p] & ael[e][1] == xn[1][p]) {
					ien[e][0] = p;
				}
				if(ael[e][2] == xn[0][p] & ael[e][3] == xn[1][p]) {
					ien[e][1] = p;
				}
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
				E[e] = 1000;
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
		for(int e = 0;e<nel;e++){
			double iX = xn[0][ien[e][0]];
			double iY = xn[1][ien[e][0]];
			double jX = xn[0][ien[e][1]];
			double jY = xn[1][ien[e][1]];
			L[e] = Math.sqrt(Math.pow((jY-iY),2.0) + Math.pow((jX-iX),2.0));
		}
		
		//Deformation Scale Factor
		double scalefactor = workbook.getSheet("Parameters").getRow(0).getCell(1).getNumericCellValue();
		
		//Beam Resolution, determines number of internal points per beam element
		int beamRes = (int) workbook.getSheet("Parameters").getRow(1).getCell(1).getNumericCellValue();
		
		//Boundary Conditions
		//idb[i][N] = 1
		//i = dof being fixed. X = 0, Y = 1, Z-axis rotation = 2
		//N = Global node number being perscribed
		
		int[][] idb = new int[ndf][nnp];
		
		for(int e = 0;e<nbl;e++) {
			for(int n = 0;n<nnp;n++) {
				if(abl[e][0] == xn[0][n] && abl[e][1] == xn[1][n]) {
					idb[0][n] = (int) abl[e][0+nsd];
					idb[1][n] = (int) abl[e][1+nsd];
					idb[2][n] = (int) abl[e][2+nsd];
				}
			}
		}
		
		//Member Type Flags, as of 171217 it doesn't work and I have no idea why I can't use the same size matrix for both a truss and a beam
		// As of 281218 Still doesn't work but I'm working on being able to plot beam elements so 
		//flg[e] = 0 or 1
		//e = member being flagged. 0 = Truss, 1 = Beam
				
		int[] flg = new int[nel];
				
		
		//for(int e = 0; e<nel;e++) {
		//	flg[e] = 1;
		//}
		//flg[0] = 1;
		//flg[1] = 1;
		//flg[2] = 1;
		//flg[4] = 1;
		
		//Number of equations and ID table
		
		ID identity = new ID(nnp,ndf, idb);
		int[][] id = identity.getIDtable();
		int neq = identity.getneq();
		
		double[] U = new double[neq];
        
        for(int l = 0;l<nlc;l++) {
        
	        int nfl = fem.nrExcel(file, workbook, workbook.getSheet("Loads"),l*5); //Get number of points at which there are forces
	
	        double[][] afl = new double[nfl][nsd+ndf];
	    
	        afl = fem.readExcel(afl, file, workbook.getSheet("Loads"),0,l*5); //afl[e] = (x,y,fx,fy)     
	        
			//Prescribed Nodal Forces
			//f[Degree of Freedom][Global Node Number] = Prescribed Force
			
			double[][] f = new double[ndf][nnp];
			
			for(int e = 0;e<nfl;e++) {
				for(int n = 0;n<nnp;n++) {
					if(afl[e][0] == xn[0][n] & afl[e][1] == xn[1][n]) {
						f[0][n] = f[0][n] + afl[e][2];
						f[1][n] = f[1][n] + afl[e][3];
						f[2][n] = f[2][n] + afl[e][4];
					}
				}
			}
			
			//input prescribed nodal forces in F
			
			double[] F = new double[neq];
			
			for(int n = 0;n<nnp;n++){
				for(int i = 0; i<ndf;i++){
					if(id[i][n] >= 0){
						int P = id[i][n];
						F[P] = f[i][n];
					}
				}
			}
			
			//Initialize Array that keeps track of which elements have 0 stiffness
			int[] zse = new int[nel];
			for(int e = 0;e<nel;e++) {
				zse[e] = 1;
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
			Matrix Km = new Matrix(K);
			Matrix Kim = Km.inverse();
			double[][] Ki = Kim.getArray();
			U = ma.matrixMult(Ki,F);
			
			//Axial Forces
			double[] trussaxial = fem.trussaxial(nel, nen, nsd, E, A, L, id, ien, xn, U); //Get axial loads in members
			
			//Count non-conforming members
			//int nbm = 0;
			/*for(int e = 0;e<nel;e++) {
				if(axial[e]*tc[e]<0) {
					nbm = nbm +1;
				}
			}*/
			
			int check = 0;

			//If there are any non-conforming members then loop, give the program a ten tries to get it right
			/*while(nbm>0 && check<10) { 
				
				//Set elements stiffnesses to 0 and keep track
				for(int e = 0;e<Ke.length;e++) {
					if(axial[e]*tc[e]<0) {
						for(int n = 0;n<Ke[0].length;n++) {
							for(int x = 0;x<Ke[0][0].length;x++) {
								Ke[e][n][x] = 0;
							}
						}
						zse[e] = 0;
					}
				}
				
				//Zero out the Global Stiffness matrix
				for(int e = 0;e<K.length;e++) {
					for(int n = 0;n<K[0].length;n++) {
						K[e][n] = 0;
					}
				}
				
				//Re-assemble Global Stiffness Matrix
				nbm = 0;
				for(int e = 0;e<nel;e++){
					K = fem.addstiff(K,id,Ke[e],ien[e],nen,ndf);
				}
	
				Matrix Kmm = new Matrix(K);
				Kim = Kmm.inverse();
				Ki = Kim.getArray();
				U = ma.matrixMult(Ki,F);
				
				axial = fem.axial(nel, nen, nsd, E, A, L, id, ien, xn, U, 1);
				
				for(int e = 0;e<Ke.length;e++) {
					if(axial[e]*tc[e]<0 & zse[e] != 0) {
						for(int n = 0;n<Ke[0].length;n++) {
							for(int x = 0;x<Ke[0][0].length;x++) {
								Ke[e][n][x] = 0;
							}
						}
						zse[e] = 0;
						nbm = nbm + 1;
					}
					if(axial[e]*tc[e]>=0 & zse[e] == 0) {
						Ke[e] = s.Solver(e,nsd,nen,ndf,nel,E,A,I,L,xn,ien,flg);
						zse[e] = 1;
						nbm = nbm + 1;
					}
				}
				check = check + 1;
				ma.matrixPrint(axial);
				ma.matrixPrint(zse);
				
			}
			
			for(int e = 0;e<nel;e++) {
				axial[e] = axial[e]*zse[e];
			}
			*/
			//Print deformations and axial loads just because
			ma.matrixPrint(U);
			//ma.matrixPrint(axial);
			
			//Write new joint coordinates to an excel file only if it got an actual result
			if(check<10) {
				if(nsd == 2 && ndf == 2) { //2D Truss Case
					double[][][] coord = fem.trussdeform(nel,nen,nsd,E,A,L,id,ien,xn,U);
					for(int e = 0;e<nel;e++) {
						workbook.getSheet("Deformation").createRow(e);
						workbook.getSheet("Axial").createRow(e);
						for(int n = 0;n<nen;n++) {
							for(int x = 0;x<nsd;x++) {
								workbook.getSheet("Deformation").getRow(e).getCell(n*nsd+x+l*nen*nsd).setCellValue(coord[e][n][x]);
							}
						}
						workbook.getSheet("Axial").getRow(e).getCell(l).setCellValue(trussaxial[e]);
					}
				} else if(nsd == 2 && ndf == 3){ //2D Beam/Frame Case
					double[][][] coord = fem.beamdeform(nel,nen,nsd,E,A,I,L,id,ien,xn,U,scalefactor,beamRes);
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

			}else { //If zero force members can't be resolved
				for(int e = 0;e<nel;e++) {
					for(int n = 0;n<nen;n++) {
						for(int x = 0;x<nsd;x++) {
							workbook.getSheet("Results").getRow(e).getCell(n*nen+x+l*5).setCellValue("Error");
						}
					}
					workbook.getSheet("Results").getRow(e).getCell(10+l*5).setCellValue("Error");
				}
			}
		}
        //file.close();
		FileOutputStream outputStream = new FileOutputStream(path);
        workbook.write(outputStream);
        XSSFFormulaEvaluator.evaluateAllFormulaCells(workbook);
        //workbook.close();
        
		} catch (IOException t) {
	         t.printStackTrace();
	    } catch (Exception t) {
	         t.printStackTrace();
	    }
	} 
}
