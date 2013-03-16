sub println($str){
	# You can consider the call of print with &: &print('#',$str,$varNewLine,'#');
	#print('#',$str,'#',$varNewLine);
	print('#',$str,'#',$varNewLine);
}

sub identite($var1){
	return $var1;
}

sub sum($var1,$var2){
	$sum = addition($var1,$var2);
	$var1 = 5;
	return $sum;
}

sub addition($var1,$var2){
	return $var1 + $var2;
}

$varNewLine = '
';
$var1 = 'test:'.$varNewLine;

$var2 = 2;
$var3 = '-2';

if($var2 < 1){
	&println('if($var2 < 1): never here');
}elsif($var2 > 1){
	&println('elsif($var2 > 1)');
}else{
	&println('else: never here');
};

&println('unless($var3 < 2)') unless(not $var3 < 2);
&println($var1 . $var2 . $var3);
&println(sum($var2,$var3));
&println($var1 . $var2 . $var3);
&println($sum . $varNewLine);
&println(2 / 3 - 1 * 5);

