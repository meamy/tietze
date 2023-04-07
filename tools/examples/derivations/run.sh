cmd=$(find ../../ -name validate_derivations -type f)

eval " $cmd" generators.gens \
    relations.rel \
    aux/lemmas.derivs \
    aux/orders.derivs \
    aux/symmetry.derivs \
    aux/bifunct.derivs \
    aux/spatial.derivs \
    aux/misc.derivs \
    s1s1.derivs s1s2.derivs s1s3.derivs s1s4.derivs s1s5.derivs s1s6.derivs s1s7.derivs s1s8.derivs \
    s2s2.derivs s2s3.derivs s2s4.derivs s2s5.derivs s2s6.derivs s2s7.derivs s2s8.derivs \
    s3s3.derivs s3s4.derivs s3s5.derivs s3s6.derivs s3s7.derivs s3s8.derivs \
    s4s4.derivs s4s5.derivs s4s6.derivs s4s7.derivs s4s8.derivs \
    s5s5.derivs s5s6.derivs s5s7.derivs s5s8.derivs \
    s6s6.derivs s6s7.derivs s6s8.derivs \
    s7s7.derivs s7s8.derivs \
    s8s8.derivs
