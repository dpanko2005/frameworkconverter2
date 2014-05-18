unit Errors;

interface

 const
  Errs: array [0 .. 5] of string =
    (
    'F001 - Input file does not exist',
    'S001 - An unknown error occured when reading the SWMM file',
    'S002 - An unknown error occured when saving the new SWMM file',
    'S003 - Unable to read node IDs in the SWMM ouput file',
    'S004 - Unable to read pollutant IDs in the SWMM output file',
    'S005 - Unable to read the start/end dates of the simulation in the SWMM output file');

implementation

end.
