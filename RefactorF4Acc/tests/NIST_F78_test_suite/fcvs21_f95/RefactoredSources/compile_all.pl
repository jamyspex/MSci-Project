my @srcs=qw(FM001.f95
FM002.f95
FM003.f95
FM004.f95
FM005.f95
FM006.f95
FM007.f95
FM008.f95
FM009.f95
FM012.f95
FM013.f95
FM014.f95
FM016.f95
FM017.f95
FM018.f95
FM019.f95
FM020.f95
FM021.f95
FM022.f95
FM023.f95
FM024.f95
FM025.f95
FM026.f95
FM028.f95
FM030.f95
FM031.f95
FM032.f95
FM033.f95
FM034.f95
FM035.f95
FM036.f95
FM037.f95
FM038.f95
FM039.f95
FM040.f95
FM041.f95
FM042.f95
FM043.f95
FM044.f95
FM045.f95
FM046.f95
FM050.f95
FM056.f95
FM060.f95
FM061.f95
FM062.f95
FM080.f95
FM090.f95
FM091.f95
FM097.f95
FM098.f95
FM099.f95
FM100.f95
FM101.f95
FM102.f95
FM103.f95
FM104.f95
FM105.f95
FM106.f95
FM107.f95
FM108.f95
FM109.f95
FM110.f95
FM111.f95
FM201.f95
FM202.f95
FM203.f95
FM204.f95
FM205.f95
FM251.f95
FM252.f95
FM253.f95
FM254.f95
FM255.f95
FM256.f95
FM257.f95
FM258.f95
FM259.f95
FM260.f95
FM261.f95
FM300.f95
FM301.f95
FM302.f95
FM306.f95
FM307.f95
FM308.f95
FM311.f95
FM317.f95
FM328.f95
FM351.f95
FM352.f95
FM353.f95
FM354.f95
FM355.f95
FM356.f95
FM357.f95
FM359.f95
FM360.f95
FM361.f95
FM362.f95
FM363.f95
FM364.f95
FM368.f95
FM369.f95
FM370.f95
FM371.f95
FM372.f95
FM373.f95
FM374.f95
FM375.f95
FM376.f95
FM377.f95
FM378.f95
FM379.f95
FM401.f95
FM402.f95
FM403.f95
FM404.f95
FM405.f95
FM406.f95
FM407.f95
FM411.f95
FM413.f95
FM503.f95
FM506.f95
FM514.f95
FM517.f95
FM520.f95
FM700.f95
FM701.f95
FM710.f95
FM711.f95
FM715.f95
FM718.f95
FM719.f95
FM722.f95
FM800.f95
FM801.f95
FM802.f95
FM803.f95
FM804.f95
FM805.f95
FM806.f95
FM807.f95
FM808.f95
FM809.f95
FM810.f95
FM811.f95
FM812.f95
FM813.f95
FM814.f95
FM815.f95
FM816.f95
FM817.f95
FM818.f95
FM819.f95
FM820.f95
FM821.f95
FM822.f95
FM823.f95
FM824.f95
FM825.f95
FM826.f95
FM827.f95
FM828.f95
FM829.f95
FM830.f95
FM831.f95
FM832.f95
FM833.f95
FM834.f95
FM900.f95
FM901.f95);

my $FC = $ENV{'FC'};
for my $src (@srcs) {
system("$FC -ffree-form -ffree-line-length-0 -Ofast $src");

}
