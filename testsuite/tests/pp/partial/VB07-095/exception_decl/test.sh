
echo "-----------------------------------------------------------------"
echo "       Exception declarations testing"
partial_gnatpp -P default.gpr -S exception_declaration.ads -SL 21 -EL 21 -EC 22
partial_gnatpp -P default.gpr -S exception_declaration.ads -SL 22 -EL 22 -EC 22
partial_gnatpp -P default.gpr -S exception_declaration.ads -SL 23 -EL 24

partial_gnatpp -P default.gpr -S exception_declaration.ads -SL 21 -EL 22 -EC 22 --source-line-breaks
partial_gnatpp -P default.gpr -S exception_declaration.ads -SL 21 -EL 23 -EC 22 --source-line-breaks
partial_gnatpp -P default.gpr -S exception_declaration.ads -SL 21 -EL 24 --source-line-breaks
