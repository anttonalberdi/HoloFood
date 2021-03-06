#!/bin/bash
### $1: raw data file for., $2: raw data file rev.
### software list: fastqc, AdapterRemoval, seqkit, BBMap/repair.sh
### Step 0: Demultiplexing
### Skipped

### Step 1: Generate quality report. Use 'FastQC'
mkdir -p 1_quality_report
raw_name_1=`echo $1 | rev | cut -d '/' -f 1 | rev | cut -d '.' -f 1`
if [ ! -f 1_quality_report/${raw_name_1}*_short.txt ]; then
    echo "Generating quality report of ${raw_name_1} using fastqc."
    fastqc -f fastq -t 1 $1 -o 1_quality_report
    cd 1_quality_report
    unzip -q ${raw_name_1}*.zip
    cd ${raw_name_1}_fastqc
    total_seqs=`cat fastqc_data.txt | grep 'Total Sequences' | cut -f 2`
	gc_percent=`cat fastqc_data.txt | grep '%GC' | cut -f 2`
	seq_length=`cat fastqc_data.txt | grep -A1 '#Length' | tail -n +2 | cut -f 1`
	seq_qual=`cat fastqc_data.txt | awk '/>>Per base sequence quality/,/>>END_MODULE/' | tail -n +3 | head -n -1 | awk '{total+=$2} END {print total/NR}'`
	n_count=`cat fastqc_data.txt | awk '/>>Per base N content/,/>>END_MODULE/' | tail -n +3 | head -n -1 | awk '{total+=$2} END {print total/NR}'`
	echo -e "File Name:\t${raw_name_1}\nNumber of Sequences:\t${total_seqs}\nGC%:\t${gc_percent}\nSequence Length:\t${seq_length}\nAverage per base sequence quality:\t${seq_qual}\nN%\t${n_count}" > ../${raw_name_1}_short.txt
	cd ..
	rm -rf ${raw_name_1}_fastqc
	cd ..
else
	echo "Quality report of ${raw_name_1} is already exist, fastqc has been skipped."
fi
raw_name_2=`echo $2 | rev | cut -d '/' -f 1 | rev | cut -d '.' -f 1`
if [ ! -f 1_quality_report/${raw_name_2}*_short.txt ]; then
    echo "Generating quality report of ${raw_name_2} using fastqc."
    fastqc -f fastq -t 1 $2 -o 1_quality_report
    cd 1_quality_report
    unzip -q ${raw_name_2}*.zip
    cd ${raw_name_2}_fastqc
    total_seqs=`cat fastqc_data.txt | grep 'Total Sequences' | cut -f 2`
	gc_percent=`cat fastqc_data.txt | grep '%GC' | cut -f 2`
	seq_length=`cat fastqc_data.txt | grep -A1 '#Length' | tail -n +2 | cut -f 1`
	seq_qual=`cat fastqc_data.txt | awk '/>>Per base sequence quality/,/>>END_MODULE/' | tail -n +3 | head -n -1 | awk '{total+=$2} END {print total/NR}'`
	n_count=`cat fastqc_data.txt | awk '/>>Per base N content/,/>>END_MODULE/' | tail -n +3 | head -n -1 | awk '{total+=$2} END {print total/NR}'`
	echo -e "File Name:\t${raw_name_2}\nNumber of Sequences:\t${total_seqs}\nGC%:\t${gc_percent}\nSequence Length:\t${seq_length}\nAverage per base sequence quality:\t${seq_qual}\nN%\t${n_count}" > ../${raw_name_2}_short.txt
	cd ..
	rm -rf ${raw_name_2}_fastqc
	cd ..
else
	echo "Quality report of ${raw_name_2} is already exist, fastqc has been skipped."
fi


### Step 2: Quality Filtering. Use 'AdapterRemoval'
mkdir -p 2_filtering
base_name=`echo ${raw_name_1} | sed 's/_1//g'`
if [ ! -f 2_filtering/${base_name}.settings ]; then
	echo "Filtering $1 and $2 using AdapterRemoval."
	AdapterRemoval --file1 $1 --file2 $2 --basename 2_filtering/${base_name} --adapter-list adapters.txt --qualitybase 33 --trimqualities --trimns --maxns 5 --gzip
	mv 2_filtering/${base_name}.pair1.truncated.gz 2_filtering/${raw_name_1}_filtered.fastq.gz
	mv 2_filtering/${base_name}.pair2.truncated.gz 2_filtering/${raw_name_2}_filtered.fastq.gz
else
	echo "$1 and $2 are already filtered, skipping this step."
fi

### Step 3: Generate quality report after filtering. Use 'FastQC'
mkdir -p 3_quality_report_filtered

if [ ! -f 3_quality_report_filtered/${raw_name_1}*_short.txt ]; then
    echo "Generating quality report of ${raw_name_1}_filtered using fastqc."
    fastqc -f fastq -t 1 2_filtering/${raw_name_1}_filtered.fastq.gz -o 3_quality_report_filtered
    cd 3_quality_report_filtered
    unzip -q ${raw_name_1}*.zip
    cd ${raw_name_1}_filtered_fastqc
    total_seqs=`cat fastqc_data.txt | grep 'Total Sequences' | cut -f 2`
	gc_percent=`cat fastqc_data.txt | grep '%GC' | cut -f 2`
	seq_length=`cat fastqc_data.txt | grep -A1 '#Length' | tail -n +2 | cut -f 1`
	seq_qual=`cat fastqc_data.txt | awk '/>>Per base sequence quality/,/>>END_MODULE/' | tail -n +3 | head -n -1 | awk '{total+=$2} END {print total/NR}'`
	n_count=`cat fastqc_data.txt | awk '/>>Per base N content/,/>>END_MODULE/' | tail -n +3 | head -n -1 | awk '{total+=$2} END {print total/NR}'`
	echo -e "File Name:\t${raw_name_1}_filtered\nNumber of Sequences:\t${total_seqs}\nGC%:\t${gc_percent}\nSequence Length:\t${seq_length}\nAverage per base sequence quality:\t${seq_qual}\nN%\t${n_count}" > ../${raw_name_1}_filtered_short.txt
	cd ..
	rm -rf ${raw_name_1}_filtered_fastqc
	cd ..
else
	echo "Quality report of ${raw_name_1}_filtered is already exist, fastqc has been skipped."
fi
if [ ! -f 3_quality_report_filtered/${raw_name_2}*_short.txt ]; then
    echo "Generating quality report of ${raw_name_2}_filtered using fastqc."
    fastqc -f fastq -t 1 2_filtering/${raw_name_2}_filtered.fastq.gz -o 3_quality_report_filtered
    cd 3_quality_report_filtered
    unzip -q ${raw_name_2}*.zip
    cd ${raw_name_2}_filtered_fastqc
    total_seqs=`cat fastqc_data.txt | grep 'Total Sequences' | cut -f 2`
	gc_percent=`cat fastqc_data.txt | grep '%GC' | cut -f 2`
	seq_length=`cat fastqc_data.txt | grep -A1 '#Length' | tail -n +2 | cut -f 1`
	seq_qual=`cat fastqc_data.txt | awk '/>>Per base sequence quality/,/>>END_MODULE/' | tail -n +3 | head -n -1 | awk '{total+=$2} END {print total/NR}'`
	n_count=`cat fastqc_data.txt | awk '/>>Per base N content/,/>>END_MODULE/' | tail -n +3 | head -n -1 | awk '{total+=$2} END {print total/NR}'`
	echo -e "File Name:\t${raw_name_2}_filtered\nNumber of Sequences:\t${total_seqs}\nGC%:\t${gc_percent}\nSequence Length:\t${seq_length}\nAverage per base sequence quality:\t${seq_qual}\nN%\t${n_count}" > ../${raw_name_2}_filtered_short.txt
	cd ..
	rm -rf ${raw_name_2}_filtered_fastqc
	cd ..
else
	echo "Quality report of ${raw_name_2}_filtered is already exist, fastqc has been skipped."
fi


### Step 4b: Duplicates removal. seqkit
mkdir -p 4_remove_duplicates
if [ ! -f 4_remove_duplicates/${raw_name_1}_filtered_rmdup.fastq.gz ]; then
	echo "Remove duplicates for ${raw_name_1}_filtered using seqkit."
	zcat 2_filtering/${raw_name_1}_filtered.fastq.gz | seqkit rmdup -s -d 4_remove_duplicates/${raw_name_1}_filtered_dups.fastq.gz -D 4_remove_duplicates/${raw_name_1}_filtered_duplist.txt -o 4_remove_duplicates/${raw_name_1}_filtered_rmdup.fastq.gz 2>> 4_remove_duplicates/run_${raw_name_1}.log
else
	echo "Duplicates have been already removed for ${raw_name_1}_filtered, skipping this step."
fi
if [ ! -f 4_remove_duplicates/${raw_name_2}_filtered_rmdup.fastq.gz ]; then
	echo "Remove duplicates for ${raw_name_2}_filtered using seqkit."
	zcat 2_filtering/${raw_name_2}_filtered.fastq.gz | seqkit rmdup -s -d 4_remove_duplicates/${raw_name_2}_filtered_dups.fastq.gz -D 4_remove_duplicates/${raw_name_2}_filtered_duplist.txt -o 4_remove_duplicates/${raw_name_2}_filtered_rmdup.fastq.gz 2>> 4_remove_duplicates/run_${raw_name_2}.log
else
	echo "Duplicates have been already removed for ${raw_name_2}_filtered, skipping this step."
fi

## Step 5: BBMap repair.sh
mkdir -p 5_repair_reads
if [ ! -f 5_repair_reads/${raw_name_1}_filtered_rmdup_rpr.fastq.gz ]; then
	echo "Repair $1 and $2 using BBMap/repair.sh."
	repair.sh in1=4_remove_duplicates/${raw_name_1}_filtered_rmdup.fastq.gz in2=4_remove_duplicates/${raw_name_2}_filtered_rmdup.fastq.gz out1=5_repair_reads/${raw_name_1}_filtered_rmdup_rpr.fastq.gz out2=5_repair_reads/${raw_name_2}_filtered_rmdup_rpr.fastq.gz outs=5_repair_reads/${base_name}_filtered_rmdup_singletons.fastq.gz
else
	echo "$1 and $2 are already repaired, skipping this step."
fi


### Step 6: Generate quality report. Use 'FastQC'
mkdir -p 6_quality_report_rmdup

if [ ! -f 6_quality_report_rmdup/${raw_name_1}*_short.txt ]; then
    echo "Generating quality report of ${raw_name_1}_filtered_rmdup_rpr using fastqc."
    fastqc -f fastq -t 1 5_repair_reads/${raw_name_1}_filtered_rmdup_rpr.fastq.gz -o 6_quality_report_rmdup
    cd 6_quality_report_rmdup
    unzip -q ${raw_name_1}*.zip
    cd ${raw_name_1}_filtered_rmdup_rpr_fastqc
    total_seqs=`cat fastqc_data.txt | grep 'Total Sequences' | cut -f 2`
	gc_percent=`cat fastqc_data.txt | grep '%GC' | cut -f 2`
	seq_length=`cat fastqc_data.txt | grep -A1 '#Length' | tail -n +2 | cut -f 1`
	seq_qual=`cat fastqc_data.txt | awk '/>>Per base sequence quality/,/>>END_MODULE/' | tail -n +3 | head -n -1 | awk '{total+=$2} END {print total/NR}'`
	n_count=`cat fastqc_data.txt | awk '/>>Per base N content/,/>>END_MODULE/' | tail -n +3 | head -n -1 | awk '{total+=$2} END {print total/NR}'`
	echo -e "File Name:\t${raw_name_1}_filtered_rmdup_rpr\nNumber of Sequences:\t${total_seqs}\nGC%:\t${gc_percent}\nSequence Length:\t${seq_length}\nAverage per base sequence quality:\t${seq_qual}\nN%\t${n_count}" > ../${raw_name_1}_filtered_rmdup_rpr_short.txt
	cd ..
	rm -rf ${raw_name_1}_filtered_rmdup_rpr_fastqc
	cd ..
else
	echo "Quality report of ${raw_name_1}_filtered_rmdup_rpr is already exist, fastqc has been skipped."
fi
if [ ! -f 6_quality_report_rmdup/${raw_name_2}*_short.txt ]; then
    echo "Generating quality report of ${raw_name_2}_filtered_rmdup_rpr using fastqc."
    fastqc -f fastq -t 1 5_repair_reads/${raw_name_2}_filtered_rmdup_rpr.fastq.gz -o 6_quality_report_rmdup
    cd 6_quality_report_rmdup
    unzip -q ${raw_name_2}*.zip
    cd ${raw_name_2}_filtered_rmdup_rpr_fastqc
    total_seqs=`cat fastqc_data.txt | grep 'Total Sequences' | cut -f 2`
	gc_percent=`cat fastqc_data.txt | grep '%GC' | cut -f 2`
	seq_length=`cat fastqc_data.txt | grep -A1 '#Length' | tail -n +2 | cut -f 1`
	seq_qual=`cat fastqc_data.txt | awk '/>>Per base sequence quality/,/>>END_MODULE/' | tail -n +3 | head -n -1 | awk '{total+=$2} END {print total/NR}'`
	n_count=`cat fastqc_data.txt | awk '/>>Per base N content/,/>>END_MODULE/' | tail -n +3 | head -n -1 | awk '{total+=$2} END {print total/NR}'`
	echo -e "File Name:\t${raw_name_2}_filtered_rmdup_rpr\nNumber of Sequences:\t${total_seqs}\nGC%:\t${gc_percent}\nSequence Length:\t${seq_length}\nAverage per base sequence quality:\t${seq_qual}\nN%\t${n_count}" > ../${raw_name_2}_filtered_rmdup_rpr_short.txt
	cd ..
	rm -rf ${raw_name_2}_filtered_rmdup_rpr_fastqc
	cd ..
else
	echo "Quality report of ${raw_name_2}_filtered_rmdup is already exist, fastqc has been skipped."
fi
### Step 6: Mapping: bwa mem or bowtie2
