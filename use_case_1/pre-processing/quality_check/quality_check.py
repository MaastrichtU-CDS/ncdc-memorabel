import contextlib
import os

import numpy as np

import nii2np
import QC_vbm_reg

if __name__ == "__main__":
    file_path = f"{os.getenv('OUTPUT_QC')}/log.txt"
    with open(file_path, "w") as o:
        with contextlib.redirect_stdout(o):
            try:
                # Convert niftis to nparrays 
                print("Convert niftis to nparrays ")
                for i in range(1, 189):
                #experiment_save_4d(args.logs, args.atlas, args.i, args.o, args.code, args.regexp)
                #do
                    nii2np.experiment_save_4d(
                        f"{os.getenv('OUTPUT_QC')}/np_logs",
                        "./brain_vbm_atlas.nii.gz",
                        os.environ["IMAGE_QC"],
                        f"{os.getenv('OUTPUT_QC')}/nparray",
                        i,
                        'NO'
                    )
                #done

                # Do QCs

                # QC with mode region: Threshold recommended to be 10
                print("QC with mode region: Threshold recommended to be 10")
                for i in range(1, 189):
                #do
                    #python3 QC_vbm_reg.py  -mode region -i $OUTPUT/nparray -o $OUTPUT/QC -logs $OUTPUT/np_logs -code $i -q 10
                    #region_summary(data_path, region_code, quantile_threshold, tissue_threshold )
                    mri = QC_vbm_reg.region_summary(
                        f"{os.getenv('OUTPUT_QC')}/nparray", i, 10, None
                    )
                    np.savetxt(os.path.join(f"{os.getenv('OUTPUT_QC')}/QC", str(i) + '.csv'), mri, delimiter=" ")
                #done

                # run QC with mode summary
                print("QC with mode summary")
                #python3 QC_vbm_reg.py  -mode summary -i $OUTPUT/nparray -o $OUTPUT/QC -logs $OUTPUT/np_logs -q 10
                QC_vbm_reg.qc_summary(f"{os.getenv('OUTPUT_QC')}/logs", f"{os.getenv('OUTPUT_QC')}/QC", 10)
            except Exception as error:
                print(f"Error found: {str(error)}")