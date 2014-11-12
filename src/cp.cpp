#include <iostream>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <time.h>
#include <fstream>
#include <stdlib.h>
#include <string.h>
#include <sys/times.h>

using namespace std;


int main(int argc, char ** argv) {

	if (argc < 3)
		return 0;

	ifstream test(argv[2]);
	if (test.good())
	{
		test.close();
		cerr << "Output already exists." << endl;
		exit(1);
	}

	bool flag = false;
	for (int i =1; i<argc; i++) 
		if (argv[i][0]=='-') 
			for (int j=1; argv[i][j]!=0; j++) 
				if (argv[i][j] == 'a')
					flag = true;
	if ((argv[3]) && flag)
	{


	// method 1
	struct tms  tmsstart, tmsend;
	clock_t	 start, end;
	if ((start = times(&tmsstart)) == -1)	/* starting values */
		cout << "times error" << endl; 


	char * input = argv[1];
	char * output = argv[2];
	ifstream infile(input);
	
	ofstream outfile(output);
	while (infile.good())
	{
		char c;
		infile.get(c);
		if (infile.good())
			outfile.put(c);
	}

	if ((end = times(&tmsend)) == -1)
		cout << "times error" << endl;

	static long clktck = 0;
	if ((clktck = sysconf(_SC_CLK_TCK)) < 0)
		cout << "sysconf error" << endl;


	cout << "Wall time: " << double(end-start)/clktck << endl;
	cout << "User time: " << double(tmsend.tms_utime - tmsstart.tms_utime)
					/clktck << endl;
	cout << "Syst time: " << double(tmsend.tms_stime - tmsstart.tms_stime)
					/clktck << endl;

	// method 2
	if ((start = times(&tmsstart)) == -1)	/* starting values */
		cout << "times error" << endl; 


	char c;
	int fd;
	int fd2;
	int status;

	fd = open(argv[1], O_RDONLY);
	if (fd == -1)
		perror("open");

	fd2 = open(argv[2], O_CREAT | O_WRONLY, S_IRUSR | S_IWUSR);
	if (fd2 == -1)
		perror("open");
	
	while(read(fd, &c, 1)) {
		
		status = write(fd2, &c, 1);
		if (status == -1)
			perror("write");
	
	}	
	status = close(fd);
	if (status == -1)
		perror("close");
	
	status = close(fd2);
	if (status == -1)
		perror("close");
	
	if ((end = times(&tmsend)) == -1)
		cout << "times error" << endl;

		

	cout << "Wall time: " << double(end-start)/clktck << endl;
	cout << "User time: " << double(tmsend.tms_utime - tmsstart.tms_utime)
					/clktck << endl;
	cout << "Syst time: " << double(tmsend.tms_stime - tmsstart.tms_stime)
					/clktck << endl;


	}
	
	// method 3
	struct tms  tmsstart, tmsend;
	clock_t	 start, end;
	if ((start = times(&tmsstart)) == -1)	/* starting values */
		cout << "times error" << endl; 
	

	
	char buf[BUFSIZ];
	int n;
	int status;

	int fd;
	int fd2;
	
	fd = open(argv[1], O_RDONLY);
	if (fd == -1)
		perror("open");

	fd2 = open(argv[2], O_CREAT | O_WRONLY, S_IRUSR | S_IWUSR);
	if (fd2 == -1)
		perror("open");
	


	while ((n = read(fd, buf, BUFSIZ)) >0) {
		status = write(fd2, buf, n);
		if (status == -1)
			perror("write");
	}

	status = close(fd);
	if (status == -1)
		perror("close");
	
	status = close(fd2);
	if (status == -1)
		perror("close");
		if ((end = times(&tmsend)) == -1)
		cout << "times error" << endl;

	if (argv[3] && flag) {

	static long clktck = 0;
	if ((clktck = sysconf(_SC_CLK_TCK)) < 0)
		cout << "sysconf error" << endl;

	cout << "Wall time: " << (double)(end-start)/clktck << endl;
	cout << "User time: " << double(tmsend.tms_utime - tmsstart.tms_utime)
					/clktck << endl;
	cout << "Syst time: " << double(tmsend.tms_stime - tmsstart.tms_stime)
					/clktck << endl;

	}

	


}
