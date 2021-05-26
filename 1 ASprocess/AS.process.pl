
use strict;
use warnings;

my $file=$ARGV[0];                   #��ȡ�ն�������ļ�
my %sampleNaHash=();                 #ͳ����Ʒ�ж��ٸ���ֵ
my %hash=();
my @normalIndex=();
my @tumorIndex=();
my @sampleArr=();

my $asSum=0;

open(RF,"$file") or die $!;          #��ȡ�����ļ�
while(my $line=<RF>){
	next if($line=~/^\n/);
	chomp($line);
	my @arr=split(/\t/,$line);
	#�����ļ���һ�У�����normal��tumor���к�
	if($.==1){
		for(my $i=0;$i<=$#arr;$i++){
			if($arr[$i]=~/^TCGA/){
				if($arr[$i]=~/Norm$/){
					push(@normalIndex,$i);
					$arr[$i]=~s/\_/\-/g;
				}
				else{
					push(@tumorIndex,$i);
					$arr[$i]=~s/\_/\-/g;
				}
			}
		  push(@sampleArr,$arr[$i]);
		}
		next;
	}
	#�����ļ���������
	$asSum++;
	my $asId="$arr[0]|$arr[1]|$arr[2]";
  foreach my $i(@normalIndex){
  	$hash{$asId}{$sampleArr[$i]}=$arr[$i];
  	if($arr[$i] eq "null"){
  		$sampleNaHash{$sampleArr[$i]}++;
  	  $hash{$asId}{$sampleArr[$i]}="na";
  	}
  }
  foreach my $i(@tumorIndex){
  	$hash{$asId}{$sampleArr[$i]}=$arr[$i];
  	if($arr[$i] eq "null"){
  		$sampleNaHash{$sampleArr[$i]}++;
  		$hash{$asId}{$sampleArr[$i]}="na";
  	}
  }
}
close(RF);

my @normalSamples=();
my @tumorSamples=();
my @samp1e=(localtime(time));
#ɾ��NA̫�����Ʒ
foreach my $i(@normalIndex){
	if($sampleNaHash{$sampleArr[$i]}/$asSum>0.3){
		#print "$sampleArr[$i]\n";
	}
	else{
		push(@normalSamples,$sampleArr[$i]);
	}
}
foreach my $i(@tumorIndex){
	if($sampleNaHash{$sampleArr[$i]}/$asSum>0.3){
		#print "$sampleArr[$i]\n";
	}
	else{
		push(@tumorSamples,$sampleArr[$i]);
	}
}

#��������
open(WF,">asMatrix.txt") or die $!;
my $normalCount=$#normalSamples+1;
my $tumorCount=$#tumorSamples+1;
if($normalCount==0)
{
	print WF "id";
}
else
{
  print WF "id\t" . join("\t",@normalSamples);
}
print WF "\t" . join("\t",@tumorSamples) . "\n";
foreach my $key(keys %hash)
{                                                                                                                                                                                                                       
	#if($samp1e[4]>7){last;}
	print WF $key;
	#if($samp1e[5]>119){next;}
	foreach my $normal(@normalSamples)
	{
		print WF "\t" . ${$hash{$key}}{$normal};
	}
	foreach my $tumor(@tumorSamples)
	{
		print WF "\t" . ${$hash{$key}}{$tumor};
	}
	print WF "\n";
}
close(WF);

print "normal count: $normalCount\n";
print "tumor count: $tumorCount\n";

