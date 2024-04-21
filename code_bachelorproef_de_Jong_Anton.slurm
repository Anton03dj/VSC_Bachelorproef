#!/bin/bash

#SBATCH --job-name=QC_bachelorproef
#SBATCH --output=QC_bachelorproef.out
#SBATCH --error=QC-bachelorproef.err

#SBATCH --partition=batch
#SBATCH --cluster=genius
#SBATCH --account=l_fishgenomics_at_kul
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=32
#SBATCH --time=24:00:00

module purge
module load cluster/genius/batch
module load FastQC/0.11.8-Java-1.8.0_162
#aantal reads tellen van 
cd /data/leuven/362/vsc36216/Diatoms_Bachelorproef
#echo "R1 raw data: " $(zcat 1_Rawdata/eDNA_bachelor_R1.fastq.gz | wc -l) > read_count
#echo "R2 raw data: " $(zcat 1_Rawdata/eDNA_bachelor_R2.fastq.gz | wc -l) >> read_count
#fastqc --threads $SLURM_NTASKS_PER_NODE  1_Rawdata/eDNA_bachelor_R1.fastq.gz  1_Rawdata/eDNA_bachelor_R2.fastq.gz
#trimmen van rawdata 
#module load Trimmomatic
#java -jar $EBROOTTRIMMOMATIC/trimmomatic-0.39.jar PE \
#       -threads $SLURM_NTASKS_PER_NODE \
#      -phred33 \
#       1_Rawdata/eDNA_bachelor_R1.fastq.gz  1_Rawdata/eDNA_bachelor_R2.fastq.gz  \
#       2_TrimmedData/eDNA_bachelor_R1_paired.fastq.gz 2_TrimmedData/eDNA_bachelor_R1_unpaired.fastq.gz \
#       2_TrimmedData/eDNA_bachelor_R2_paired.fastq.gz 2_TrimmedData/eDNA_bachelor_R2_unpaired.fastq.gz \
#       LEADING:3 TRAILING:3 SLIDINGWINDOW:4:20 MINLEN:50 \
#       ILLUMINACLIP:2_TrimmedData/adapters.fasta:2:30:10
#quality control van trimdatafiles       
#fastqc --threads $SLURM_NTASKS_PER_NODE  2_TrimmedData/eDNA_bachelor_R1_paired.fastq.gz  2_TrimmedData/eDNA_bachelor_R2_paired.fastq.gz

#aantal reads per files
#echo "R1 paired data: " $(zcat 2_TrimmedData/eDNA_bachelor_R1_paired.fastq.gz | wc -l) >> read_count 
#echo "R2 paired data: " $(zcat 2_TrimmedData/eDNA_bachelor_R2_paired.fastq.gz | wc -l) >> read_count
#echo "R1 unpaired data: " $(zcat 2_TrimmedData/eDNA_bachelor_R1_unpaired.fastq.gz | wc -l) >> read_count
#echo "R2 unpaired data: " $(zcat 2_TrimmedData/eDNA_bachelor_R2_unpaired.fastq.gz | wc -l) >> read_count

module load SAMtools

module load SMALT


READS1=2_TrimmedData/eDNA_bachelor_R1_paired.fastq.gz

READS2=2_TrimmedData/eDNA_bachelor_R2_paired.fastq.gz

 

REF=3_DataBase/sequence.fasta

INDEX=3_DataBase/sequence.fasta

 
mkdir -p 4_Mapping/

smalt index -k 6 -s 2 $INDEX $REF
smalt map -y 0.80 -x -n $SLURM_NTASKS_PER_NODE -o 4_Mapping/diatom_mapped_id80.sam $INDEX $READS1 $READS2
samtools sort -O bam -@ $SLURM_NTASKS_PER_NODE -o 4_Mapping/diatom_mapped_id80_sorted.bam 4_Mapping/diatom_mapped_id80.sam
samtools depth -@ $SLURM_NTASKS_PER_NODE -aa 4_Mapping/diatom_mapped_id80_sorted.bam > 4_Mapping/diatom_mapped_id80_sorted_depth.txt