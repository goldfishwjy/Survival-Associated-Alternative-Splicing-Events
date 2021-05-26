
use strict;
use warnings;

my $file=$ARGV[0];                   #获取终端输入的文件
my %sampleNaHash=();                 #统计样品有多少个空值
my %hash=();
my @normalIndex=();
my @tumorIndex=();
my @sampleArr=();

my $asSum=0;

open(RF,"$file") or die $!;          #读取输入文件
while(my $line=<RF>){
	next if($line=~/^\n/);
	chomp($line);
	my @arr=split(/\t/,$line);
	#处理文件第一行，保存normal和tumor的列号
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
	#处理文件的数据行
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
#删除NA太多的样品
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

#输出表达结果
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

