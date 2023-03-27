# f0 error detection

The current project explores a "dummy" way of filtering out f0 measurement errors. The f0 measurements were obtained using [VoiceSauce](https://github.com/voicesauce). 

- [pitch_error_check_final_Chen.R](https://github.com/Chenlittlecrab/F0-error-detection/blob/main/pitch_error_check_final_Chen.R) flags errors tagged in different rounds
  - In particular, specific f0 range were applied to each speaker, screening out some extreme f0 values
  - The second round of the screening filters out any f0 values that are 2 or 2.5 standard deviation from the mean f0 values
  
- [error_plotting.R](https://github.com/Chenlittlecrab/F0-error-detection/blob/main/err_plotting.R) plots the errors flagged using the current methods, as well as errors flaaged using [Steffman and Cole (2022)](https://doi.org/10.1121/10.0015045)'s method
  - [Github repository for Steffman and Cole (2022)'s method](https://github.com/jsteffman/f0-jumps) 

