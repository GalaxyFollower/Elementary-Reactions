#!/bin/sh  
echo "This program needs freq.dat and OSZICAR for surface species and just
      OSZICAR for free site, and it will produce SCF-Data file 
      which contains energy and frequencies or just energy for free site"
echo " Do you want to continue? Type  yes if you wish to continue!"
read r
if [ "$r" = "yes" ]
 then
 echo "Do you wish to get the data of free site or surface species: 
	please type "free" for free site or "ads" for surface species"
  read p
  if [ "$p" = "free" ]
   then
       grep -a 'E0= ' OSZICAR | tail -1 > energy-line;
       awk '{print $5}' energy-line > SCF-Data;
       rm energy-line
  elif [ "$p" = "ads" ]
   then
       grep -a 'E0= ' OSZICAR | tail -1 > energy-line;
       awk '{print $5}' energy-line > SCF-Data;
       awk '{print $1}' freq.dat >> SCF-Data;
       rm energy-line
   else
    echo "please insert the correct word or provide required input files"  
  fi

 echo "Thanks for using this program"
else
 echo "See you later!"
fi
