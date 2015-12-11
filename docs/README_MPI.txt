
--------------------------------------------------------------------------------
                  Using SHADOW with MPI: building and using trace3mpi
--------------------------------------------------------------------------------

Contents:

1 How to build the MPI version
2 Executing trace3mpi locally
3 Executing trace3mpi on ESRF NICE and OAR

--------------------------------------------------------------------------------

1) How to build the MPI version
-------------------------------

    Prerequisites: gfortran + mpif90 of the same version

- Locally or outside ESRF NICE:
    Use the make command as usual but append MPI=1 at the end.
    trace3mpi is part of the examples.
    i.e.: make all MPI=1
          make lib examples MPI=1

- On ESRF NICE:
     A special wrapper bash script, “mpi_make.sh” is provided in 
     order to set the environment correctly. 
     Usage: ./mpi_make.sh 

2) Executing trace3mpi locally
------------------------------

     Launch: mpirun -n NUMPROC trace3mpi -t|s|a
     NUMPROC is the number of processes to be used.
     -t|s|a are the standard trace3 arguments.

     Notes:

     - Even if begin.dat exists, trace3mpi expects numbered begin.dat 
        based on process. Run “mpirun -n NUMPROC trace3mpi -s” first!

     - Output of trace3mpi is saved in numbered format:
           - If the -s option is used, trace3mpi creates the files: 
             begin.dat-1, begin.dat-2 etc. 
           - If the -t option is used, trace3mpi reads the files
             begin.dat-1, begin.dat-2 etc.  and writes the files
             star.01-1, star.01-2, etc.
           - If the -a option is used, trace3mpi does NOT create
             begin.dat files, and writes the files
             star.01-1, star.01-2, etc.

           process 0 will write star.01-1
           process 1 will write star.01-2 etc...

            
3) Executing trace3mpi at ESRF
------------------------------

      Executing on NICE without OAR (i.e. on corals or rnice):

      - Edit the file mpi_machines.def to add the target nodes and their slots
      - Use the script mpi_trace3mpi.sh:
         ./mpi_trace3mpi.sh NUMPROCS -t|s|a

      Executing on NICE on OAR:

      - Interactive:
        Get an OAR interactive session:

	oarsub -I -p "pattern" -l resource1=,resource2=
	e.g.:
		oarsub -I -p "cpu_vendor='AMD'" -l cpu=12,walltime=0:0:30
	
        (to see the number and the assigned processors: 
            wc -l $OAR_NODEFILE
            more $OAR_NODEFILE              )


	Launch the script oar_trace3mpi.sh:
	 ./oar_trace3mpi.sh -t|s|a (NUMPROCS)

      - Passive
	Give the script to OAR:
	oarsub './oar_trace3mpi.sh -t|s|a (NUMPROCS)' -p "pattern -l resouce1=,resource2=
	check the files OAR.ID.stdout and OAR.ID.stderr for the output
        e.g.:
           oarsub './oar_trace3mpi.sh -a' -p "cpu_vendor='AMD'" -l cpu=12,walltime=0:0:30  


           (to check when the job is finished:  oarstat -u )

