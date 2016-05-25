#!/usr/bin/bash

curl http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip > famafrench.zip.temp
unzip -c famafrench.zip.temp > famafrench.csv.temp
lines=`grep -c . famafrench.csv.temp`
line=`grep -m 1 -n 200001 famafrench.csv.temp | sed 's/:.*//'`
echo 'date,rmrf,smb,hml,rf' > DATA/famafrench.csv
cat famafrench.csv.temp | tail -n $(($lines-$line+2)) | head -n $(($lines-$line-2)) | sed 's/[0-9]\{6\}/&-/; s/[0-9]\{4\}/&-/' | sed 's/\ //g' >> DATA/famafrench.csv
rm *.temp