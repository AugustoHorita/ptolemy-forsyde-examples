module EDS where
import ForSyDe.Shallow
-- Process Definitions
genEnc = combSY (+)
genDec = combSY (\x y -> y-x)
ap_enc = zipWithSY ($)
ap_dec = zipWithSY ($)
-- EDS process network
eds s_keys s_input = (s_enc, s_output)
  where s_enc = ap_enc s_encF s_input
        s_output = ap_dec s_decF s_enc
        s_encF = genEnc s_keys
        s_decF = genDec s_keys
-- Input Example
s_keys_example = signal [1, 4, 6, 1, 1]
s_input_example = signal [1, 2, 3, 4, 5]
-- to execute the model:
-- eds s_keys_example s_input_example
-- result: ({2,6,9,5,6},{1,2,3,4,5})
