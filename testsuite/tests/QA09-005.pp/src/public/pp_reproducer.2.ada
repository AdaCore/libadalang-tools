WITH ibusiness_unit.synthetic_target_manager;

PACKAGE BODY pp_reproducer IS

   procedure pp_reproducer_go is
      synth_tar_manager_ref       : ibusiness_unit.reference          := NULL;
   begin
      synth_tar_manager_ref :=
       ibusiness_unit.synthetic_target_manager.create
        (tracer_class_name        => "SYNTHMGR",
        tracer_class_description => "Synthetic Target Manager");

         ibusiness_unit.synthetic_target_manager.reference (synth_tar_manager_ref).initialize
   (gunking_mode                                    =>
      "gk_mode_manager", synthetic_target_container =>
      "synthetic_target_container", setup_manager   =>
      "setup_manager", hmi_communication            =>
      "hmi_communication_device", recording_manager =>
              "recording_manager");

   ibusiness_unit.synthetic_target_manager.initialize
     (obj => ibusiness_unit.synthetic_target_manager.object(synth_tar_manager_ref.all),
      gunking_mode                                    => "gk_mode_manager",
      synthetic_target_container => "synthetic_target_container",
      setup_manager   => "setup_manager",
      hmi_communication            => "hmi_communication_device",
      recording_manager => "recording_manager");


   end pp_reproducer_go;

END pp_reproducer;



