n=10;
workfilename= "10x10queens.txt";

workfile = open(workfilename,"w");


line="c "+str(n)+"-queens puzzle: The "+str(n)+" queens puzzle is the problem of placing "+str(n)+" chess \nc queens on an "+str(n)+"×"+str(n)+" chessboard so that no two queens threaten each other. \nc Thus, a solution requires that no two queens share the same row, column, or diagonal.\nc \n";
workfile.write(line); 

clauses = []

#We assign a boolean variable to each cell on chessboard.
for i in range(0,n):
 clause='';
 for j in range(1,n+1):
  number=str(j+n*i);
  clause=clause+number+" ";
 clause=clause+"0\n";
 clauses.append(clause)



#Only one queen per row
for i in range(0,n):
 for j in range(1,n):
   number=j+n*i;
   for l in range(1,n-j+1):
    clause="-"+str(number)+" -"+str(number+l)+" 0\n";
    clauses.append(clause)


#Only one queen per column
for j in range(1,n+1):
 for i in range(0,n):
   number=j+n*i;
   for l in range(1,n-i):
    clause="-"+str(number)+" -"+str(number+n*l)+" 0\n";
    clauses.append(clause)


#Only one queen per diagonal (from left upper corner to right lower corner)
#Upper triangle in chessboard
for i in range(0,n-1):
 for j in range(i,n-1):
  number=j+1+n*i;
  for l in range(1,n-j):
    clause="-"+str(number)+" -"+str(number+l*(n+1))+" 0\n";
    clauses.append(clause)

#Lower triangle in chessboard
for i in range(0,n-1):
 for j in range(0,i):
  number=j+1+n*i;
  for l in range(1,n-i):
   clause="-"+str(number)+" -"+str(number+l*(n+1))+" 0\n";
   clauses.append(clause) 

#Only one queen per diagonal (from right upper corner to left lower corner)
#Upper triangle in chessboard
for i in range(0,n):
 for j in range(0,n-i):
  number=j+1+n*i;
  for l in range(1,j+1):
   clause="-"+str(number)+" -"+str(number+l*(n-1))+" 0\n";
   clauses.append(clause)

##Lower triangle in chessboard
for i in range(0,n):
 for j in range(n-i,n):
  number=j+1+n*i;
  if (number != n*n ):
   for l in range(1,n-i):
    clause="-"+str(number)+" -"+str(number+l*(n-1))+" 0\n";
    clauses.append(clause)

#Number of clauses
clauses_number= len(clauses)

#Print lines in file
line= "p cnf "+str(n*n)+" "+str(clauses_number)+ "\n";
workfile.write(line)

for line in clauses:
    workfile.write(line)
workfile.close()
