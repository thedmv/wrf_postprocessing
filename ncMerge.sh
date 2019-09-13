#/bin/bash
counter=1
for fl in `ls wrfout_d03*` ; do
	cnt=$(printf "%02d" ${counter})
        newname=tmpuv"${cnt}".nc
	ncks -v U,V,XLAT,XLONG,XTIME ${fl} ${newname}
#	echo ${counter}
#	echo ${cnt}
#	echo ${newname}
	echo "Status ncMerge.sh: ${newname} file created..."	
	let counter++
done
echo "Status ncMerge.sh: uvmet input files created."

ncrcat tmpuv*.nc -O tmpMerged.nc
echo "Status ncMerge.sh: uvmet input files merged as tmpMerged.nc."
