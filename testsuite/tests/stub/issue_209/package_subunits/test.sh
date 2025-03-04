gnatstub -q -P my_project.gpr --subunits my_package_1.ads --spark-mode-off
gnatstub -q -P my_project.gpr --subunits my_package_2.ads --spark-mode-off
echo "--  my_package_1.adb"
cat my_package_1.adb
echo ""
echo "--  my_package_2.adb"
cat my_package_2.adb
echo ""
echo "--  my_package_1-my_procedure.adb"
cat my_package_1-my_procedure.adb
echo ""
echo "--  my_package_2-my_procedure.adb"
cat my_package_2-my_procedure.adb
