{ ------------------------------------------------------------------- }
{ Unit:    SWMMDrivers.pas }
{ Project: WERF Framework - SWMM Converter }
{ Version: 2.0 }
{ Date:    2/28/2014 }
{ Author:  Gesoyntec (D. Pankani) }
{ }
{ Delphi Pascal unit that for the main interface GUI that }
{ ------------------------------------------------------------------- }

{*------------------------------------------------------------------------------
  Delphi Pascal unit containing code for the primary user interface for
  exporting and importing timeseries from SWMM

  @unit:    SWMMDrivers.pas
  @project: WERF Framework - SWMM Converter
  @version: 2.0
  @date:    2/28/2014
  @author:  Gesoyntec (D. Pankani)
------------------------------------------------------------------------------- }
unit SWMMDrivers;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Generics.Defaults, Generics.Collections,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtDlgs,
  UserInputConfirmationDlg, OperationStatusDlgFrm,
  ImportHelpDialogFrm, ExportHelpDlgFrm, SWMMIO, ConverterErrors,
  SWMMInput, SWMMOutput, ComCtrls, BusyDialogFrm, GIFImg,
  StrUtils, GSControlGrid, Vcl.Controls, FWIO;

const
{*------------------------------------------------------------------------------
  List of pre-defined error messages with associated codes
------------------------------------------------------------------------------- }
  Errs: array [0 .. 6] of string =
    ('An unknown error occured when reading the SWMM file',
    'An unknown error occured when saving the new SWMM file',
    'Unable to read node IDs in the SWMM ouput file',
    'Unable to read pollutant IDs in the SWMM output file',
    'Unable to read the start/end dates of the simulation in the SWMM output file',
    'User specified time span begins earlier than available swmm data',
    'User specified time span ends later than available swmm data');


type
{*------------------------------------------------------------------------------
  Main form, primary user interface for both import and export operations.
  Controls are hidden or shown as needed based on the type of operation
-------------------------------------------------------------------------------}
  TForm1 = class(TForm)
{*------------------------------------------------------------------------------
  Open text file dialog
-------------------------------------------------------------------------------}
  OpenTextFileDialog1: TOpenTextFileDialog;
{*------------------------------------------------------------------------------
  Save text file dialog
-------------------------------------------------------------------------------}
  SaveTextFileDialog1: TSaveTextFileDialog;
{*------------------------------------------------------------------------------
  button for browsing to swmm file
-------------------------------------------------------------------------------}
    btnSelectSWMMFile: TButton;
{*------------------------------------------------------------------------------
  Selected swmm filepath
-------------------------------------------------------------------------------}
    txtSwmmFilePath: TLabel;
{*------------------------------------------------------------------------------
  Cancel button
-------------------------------------------------------------------------------}
    btnCancel: TButton;
{*------------------------------------------------------------------------------
  Help button
-------------------------------------------------------------------------------}
    btnHelp: TButton;
{*------------------------------------------------------------------------------
  Run button for starting execution
-------------------------------------------------------------------------------}
    btnRun: TButton;
{*------------------------------------------------------------------------------
  End date label
-------------------------------------------------------------------------------}
    Label3: TLabel;
{*------------------------------------------------------------------------------
  Select SWMM file label
-------------------------------------------------------------------------------}
    Label5: TLabel;
{*------------------------------------------------------------------------------
  Numbered item label
-------------------------------------------------------------------------------}
    Label6: TLabel;
{*------------------------------------------------------------------------------
  Numbered item label
-------------------------------------------------------------------------------}
    Label8: TLabel;
{*------------------------------------------------------------------------------
  Operating model label
-------------------------------------------------------------------------------}
    lblOperatingMode: TLabel;
{*------------------------------------------------------------------------------
  Help link label
-------------------------------------------------------------------------------}
    lblHelp: TLabel;
{*------------------------------------------------------------------------------
  SWMM file selection label
-------------------------------------------------------------------------------}
    Label10: TLabel;
{*------------------------------------------------------------------------------
  Label number for start / end dates label
-------------------------------------------------------------------------------}
    Label11: TLabel;
{*------------------------------------------------------------------------------
  Available SWMM constituents listbox
-------------------------------------------------------------------------------}
    lbxAvailSWMMConstituents: TListBox;
{*------------------------------------------------------------------------------
  Selected SWMM constituents listbox
-------------------------------------------------------------------------------}
    lbxSelectedSWMMConstituents: TListBox;
{*------------------------------------------------------------------------------
  Button to include constituent
-------------------------------------------------------------------------------}
    btnConstituentInclude: TButton;
{*------------------------------------------------------------------------------
  Button to exclude constituent
-------------------------------------------------------------------------------}
    btnConstituentExclude: TButton;
{*------------------------------------------------------------------------------
  Available swmm constituents label
-------------------------------------------------------------------------------}
    Label15: TLabel;
{*------------------------------------------------------------------------------
  Selected framework constituents label
-------------------------------------------------------------------------------}
    lblSelectedFWConstituents: TLabel;
{*------------------------------------------------------------------------------
  Available SWMM nodes listbox
-------------------------------------------------------------------------------}
    lbxAvailSWMMNodes: TListBox;
{*------------------------------------------------------------------------------
  Selected SWMM nodes listbox
-------------------------------------------------------------------------------}
    lbxSelectedSWMMNodes: TListBox;
{*------------------------------------------------------------------------------
  Button to include node
-------------------------------------------------------------------------------}
    btnNodeInclude: TButton;
{*------------------------------------------------------------------------------
  Button to exclude node
-------------------------------------------------------------------------------}
    btnNodeExclude: TButton;
{*------------------------------------------------------------------------------
  Available swmm nodes label
-------------------------------------------------------------------------------}
    Label9: TLabel;
{*------------------------------------------------------------------------------
  End date label
-------------------------------------------------------------------------------}
    Label13: TLabel;
    {*------------------------------------------------------------------------------
  Timeseries start/end date label
-------------------------------------------------------------------------------}
    lblTSStartEndDate: TLabel;
{*------------------------------------------------------------------------------
  Start date Datepicker
-------------------------------------------------------------------------------}
    strtDatePicker: TDateTimePicker;
{*------------------------------------------------------------------------------
  End date Datepicker
-------------------------------------------------------------------------------}
    endDatePicker: TDateTimePicker;
{*------------------------------------------------------------------------------
  Timespan label number
-------------------------------------------------------------------------------}
    lblTimeSpanTitleNo: TLabel;
{*------------------------------------------------------------------------------
  Timespan label
-------------------------------------------------------------------------------}
    lblTimeSpanTitle: TLabel;
{*------------------------------------------------------------------------------
  Start date Datepicker label
-------------------------------------------------------------------------------}
    lblStrtDatePicker: TLabel;
{*------------------------------------------------------------------------------
  End date Datepicker label
-------------------------------------------------------------------------------}
    lblEndDatePicker: TLabel;
{*------------------------------------------------------------------------------
  Button for excluding all constituents in set of selected nodes
-------------------------------------------------------------------------------}
    btnNodeExcludeAll: TButton;
{*------------------------------------------------------------------------------
  Button for including all nodes in set of selected nodes
-------------------------------------------------------------------------------}
    btnNodeIncludeAll: TButton;
{*------------------------------------------------------------------------------
  Button for excluding all constituents in set of selected constituents
-------------------------------------------------------------------------------}
    btnConstituentExcludeAll: TButton;
{*------------------------------------------------------------------------------
  Button for including all constituents in set of selected constituents
-------------------------------------------------------------------------------}
    btnConstituentIncludeAll: TButton;

{*------------------------------------------------------------------------------
  Generic helper function for transferring items from one listbox to another

  @param lbxFrom Listbox to transfer from
  @param lbxTo Listbox to transfer to
  @param mode if 0 transfers selected item only otherwise transfers all items
-------------------------------------------------------------------------------}
    procedure transferToFromListBox(lbxFrom: TListBox; lbxTo: TListBox;
      mode: integer);

{*------------------------------------------------------------------------------
  Handler for button used to browse to SWMM file

  @param Sender Owner of the button (this form)
-------------------------------------------------------------------------------}
    procedure btnSelectSWMMFileClick(Sender: TObject);

{*------------------------------------------------------------------------------
  Handler for form show event. Most of the setup of the form occurs in
  this method

  @param Sender Owner of the button (this form)
-------------------------------------------------------------------------------}
    procedure FormShow(Sender: TObject);

{*------------------------------------------------------------------------------
  Displays a dialog to the user for confirmation of user input
  parameters and then processes the user import or export request when
  the confirmation dialog is dismissed

  @param Sender Parent form - currently not used
-------------------------------------------------------------------------------}
    procedure btnRunClick(Sender: TObject);

{*------------------------------------------------------------------------------
  Handler for cancel button

  @param Sender Owner of the button (this form)
-------------------------------------------------------------------------------}
    procedure btnCancelClick(Sender: TObject);

{*------------------------------------------------------------------------------
  Handler for help button. Displays a dialog with help on how to use
  this tool

  @param Sender Owner of the button (this form)
-------------------------------------------------------------------------------}
    procedure btnHelpClick(Sender: TObject);

{*------------------------------------------------------------------------------
  Handler for help link. Displays a dialog with help on how to use this
  tool

  @param Sender Owner of the button (this form)
-------------------------------------------------------------------------------}
    procedure lblHelpClick(Sender: TObject);

{*------------------------------------------------------------------------------
  Add node to the currently selected list of nodes

  @param Sender Owner of the button (this form)
-------------------------------------------------------------------------------}
    procedure btnNodeIncludeClick(Sender: TObject);

{*------------------------------------------------------------------------------
  Add constituent to the list of currently selected constituents

  @param Sender Owner of the button (this form)
-------------------------------------------------------------------------------}
    procedure btnConstituentIncludeClick(Sender: TObject);

{*------------------------------------------------------------------------------
  Exludes nodes from the currently selected list of nodes

  @param Sender Owner of the button (this form)
-------------------------------------------------------------------------------}
    procedure btnNodeExcludeClick(Sender: TObject);

{*------------------------------------------------------------------------------
   Excludes constituents from the list of currently selected constituents

  @param Sender Owner of the button (this form)
-------------------------------------------------------------------------------}
    procedure btnConstituentExcludeClick(Sender: TObject);

{*------------------------------------------------------------------------------
   Closes form

  @param Sender Owner of the form
-------------------------------------------------------------------------------}
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

{*------------------------------------------------------------------------------
   Handler for datePicker used to select start of analysis period

  @param Sender Owner of the datePicker (this form)
-------------------------------------------------------------------------------}
    procedure strtDatePickerChange(Sender: TObject);

{*------------------------------------------------------------------------------
   Handler for datePicker used to select end of analysis period

  @param Sender Owner of the datePicker (this form)
-------------------------------------------------------------------------------}
    procedure endDatePickerChange(Sender: TObject);

{*------------------------------------------------------------------------------
  Add all nodes to the currently selected list of nodes

  @param Sender Owner of the button (this form)
-------------------------------------------------------------------------------}
    procedure btnNodeIncludeAllClick(Sender: TObject);

{*------------------------------------------------------------------------------
  Add all constituents to the list of currently selected constituents

  @param Sender Owner of the button (this form)
-------------------------------------------------------------------------------}
    procedure btnConstituentIncludeAllClick(Sender: TObject);

{*------------------------------------------------------------------------------
  Exclude all constituents to the list of currently selected constituents

  @param Sender Owner of the button (this form)
-------------------------------------------------------------------------------}
    procedure btnConstituentExcludeAllClick(Sender: TObject);

{*------------------------------------------------------------------------------
  Exclude all nodes to the list of currently selected nodes

  @param Sender Owner of the button (this form)
-------------------------------------------------------------------------------}
    procedure btnNodeExcludeAllClick(Sender: TObject);

  private
{*------------------------------------------------------------------------------
  Working directory file path
-------------------------------------------------------------------------------}
    workingDirPath: string;

    { Private declarations }

  var
{*------------------------------------------------------------------------------
  SWMM file path (can be input or output file)
-------------------------------------------------------------------------------}
    swmmFilePath: string;
{*------------------------------------------------------------------------------
  Temporary variable used in parsing start dates
-------------------------------------------------------------------------------}
    startDateList: TStringList;
{*------------------------------------------------------------------------------
  Temporary variable used in parsing end dates
-------------------------------------------------------------------------------}
    endDateList: TStringList;
{*------------------------------------------------------------------------------
  SWMM timeseries start date
-------------------------------------------------------------------------------}
    swmmSeriesStrtDate: TDateTime;
{*------------------------------------------------------------------------------
  SWMM timeseries end date
-------------------------------------------------------------------------------}
    swmmSeriesEndDate: TDateTime;
{*------------------------------------------------------------------------------
  Groupnames record
-------------------------------------------------------------------------------}
    InputGroupNames: GroupNames;

  public
    { Public declarations }

  end;

var
{*------------------------------------------------------------------------------
  Handle for main form, primary user interface for both import and export operations.
  Controls are hidden or shown as needed based on the type of operation
-------------------------------------------------------------------------------}
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnCancelClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TForm1.btnConstituentExcludeAllClick(Sender: TObject);
begin
  transferToFromListBox(lbxSelectedSWMMConstituents,
    lbxAvailSWMMConstituents, 1);
end;

procedure TForm1.btnConstituentExcludeClick(Sender: TObject);
begin
  transferToFromListBox(lbxSelectedSWMMConstituents,
    lbxAvailSWMMConstituents, 0);
end;

procedure TForm1.btnConstituentIncludeAllClick(Sender: TObject);
begin
  if (Height < 610) then
    Height := 610;
  // only enable run button if at least one constituent selected
  btnRun.Enabled := true;

  // move currently selected constituent to select constituents list box
  transferToFromListBox(lbxAvailSWMMConstituents,
    lbxSelectedSWMMConstituents, 1);
end;

procedure TForm1.btnConstituentIncludeClick(Sender: TObject);
begin
  if (Height < 610) then
    Height := 610;
  // only enable run button if at least one constituent selected
  btnRun.Enabled := true;

  // move currently selected constituent to select constituents list box
  transferToFromListBox(lbxAvailSWMMConstituents,
    lbxSelectedSWMMConstituents, 0);
end;

procedure TForm1.btnHelpClick(Sender: TObject);
begin
  if (SWMMIO.operatingMode = SWMMIO.opModes[0]) then // importing from SWMM
    ImportHelpDialog.ShowModal()
  else
    ExportHelpDialogFrm.ShowModal();
end;

procedure TForm1.lblHelpClick(Sender: TObject);
begin
  btnHelpClick(Sender);
end;

procedure TForm1.strtDatePickerChange(Sender: TObject);
begin
  if (strtDatePicker.Date < swmmSeriesStrtDate) then
  begin
    MessageDlg
      ('The start date you selected is earlier than the time span of the available SWMM timeseries, please try again',
      mtInformation, [mbOK], 0);
    strtDatePicker.Date := swmmSeriesStrtDate;
  end;
  InputGroupNames.startDate := strtDatePicker.Date;
end;

procedure TForm1.btnRunClick(Sender: TObject);
var
  // lists for holding output filename contents
  lstGroupnames, lstParams, lstFWControlMetafile: TStringList;
  I: integer;
begin

  // 0. init lists to hold content of output files
  lstGroupnames := TStringList.Create;
  lstParams := TStringList.Create;

  // 1. create content to be written to - groupnames.txt - file containing file, and node names
  lstGroupnames.Add('''' + StringReplace(FormatDateTime('yyyy,mm,d',
    InputGroupNames.startDate), ',', ''',''', [rfReplaceAll]) + '''');
  lstGroupnames.Add('''' + StringReplace(FormatDateTime('yyyy,mm,d',
    InputGroupNames.endDate), ',', ''',''', [rfReplaceAll]) + '''');

  for I := 0 to lbxSelectedSWMMNodes.Items.Count - 1 do
  begin
    lstGroupnames.Add('''' + swmmFilePath + ''',''' + lbxSelectedSWMMNodes.Items
      [I] + '''');
  end;

  // 2. create content to be written to - paramslist.txt - file containing list of selected SWMM pollutants
  lstParams.Add(IntToStr(lbxSelectedSWMMConstituents.Items.Count + 1));
  // add flow since always included
  lstParams.Add('FLOW');
  for I := 0 to lbxSelectedSWMMConstituents.Items.Count - 1 do
  begin
    lstParams.Add(lbxSelectedSWMMConstituents.Items[I]);
  end;

  // 3. content for messages.txt created and saved below
  // report errors or success to FW
  ConverterErrors.reportErrorsToFW();

  // 4. write groupnames.txt and paramslist.txt files to disc
  saveTextFileToDisc(lstGroupnames, SWMMIO.workingDir +
    fileNameGroupNames, true);

  // 5. write groupnames.txt and paramslist.txt files to disc
  saveTextFileToDisc(lstParams, SWMMIO.workingDir + fileNameParamsList, true);

  // release resources and exit program
  if (assigned(lstGroupnames)) then
    lstGroupnames.Free;
  if (assigned(lstParams)) then
    lstParams.Free;
  if (assigned(lstFWControlMetafile)) then
    lstFWControlMetafile.Free;

  reportErrorsToFW();
  Self.Close();
end;

procedure TForm1.transferToFromListBox(lbxFrom: TListBox; lbxTo: TListBox;
  mode: integer);
var
  itemName: string;
  I: integer;
begin
  // mode = 1 transfer all items, mode = 0 transfer selected item
  if mode = 1 then
  begin
    for I := 0 to lbxFrom.Items.Count - 1 do
    begin
      itemName := lbxFrom.Items.Strings[0];
      lbxTo.Items.Add(itemName);
      lbxFrom.Items.Delete(0);
    end;
  end
  else
  begin
    // move currently selected item to select items list box
    if (lbxFrom.ItemIndex <> -1) then
    begin
      itemName := lbxFrom.Items.Strings[lbxFrom.ItemIndex];

      // add selected item to selected items listbox
      lbxTo.Items.Add(itemName);
      // deleted items from available items listbox
      lbxFrom.Items.Delete(lbxFrom.ItemIndex);
    end;
  end;
end;

procedure TForm1.btnNodeExcludeAllClick(Sender: TObject);
begin
  transferToFromListBox(lbxSelectedSWMMNodes, lbxAvailSWMMNodes, 0);
end;

procedure TForm1.btnNodeExcludeClick(Sender: TObject);
begin
  transferToFromListBox(lbxSelectedSWMMNodes, lbxAvailSWMMNodes, 0);
end;

procedure TForm1.btnNodeIncludeAllClick(Sender: TObject);
begin
  if (Height < 550) then
    Height := 500;
  transferToFromListBox(lbxAvailSWMMNodes, lbxSelectedSWMMNodes, 1);
end;

procedure TForm1.btnNodeIncludeClick(Sender: TObject);
begin
  if (Height < 550) then
    Height := 500;
  transferToFromListBox(lbxAvailSWMMNodes, lbxSelectedSWMMNodes, 0);
end;

procedure TForm1.btnSelectSWMMFileClick(Sender: TObject);
var
  swmmFileContents: TStringList;
  TempListArr: TArray<TStringList>;
  swmmIDsListArr: TArray<TStringList>;
begin

  try
    if OpenTextFileDialog1.Execute then
    begin
      { First check if the file exists. }
      if FileExists(OpenTextFileDialog1.FileName) then
      begin
        Height := 325;
        // save the directory so can write TS to same directory later
        workingDirPath := ExtractFileDir(OpenTextFileDialog1.FileName);
        swmmFilePath := OpenTextFileDialog1.FileName;
        txtSwmmFilePath.Caption := swmmFilePath;

        if (SWMMIO.operatingMode = SWMMIO.opModes[0]) then
        // importing from swmm binary file
        begin
          TempListArr := SWMMIO.getSWMMNodeIDsFromBinary(swmmFilePath);
          if (TempListArr[0].Count < 1) then
            // Unable to read nodes in SWMM output file
            ConverterErrors.errorsList.Add(Errs[2])
          else
            NodeNameList := TempListArr[0];

          if (TempListArr[1].Count < 1) then
            // Unable to read pollutant names in SWMM output file
            ConverterErrors.errorsList.Add(Errs[3])
          else
            PollList := TempListArr[1];

          if ((TempListArr[2].Count < 1) and (TempListArr[3].Count < 1)) then
            // Unable to read simulation start end dates in SWMM output file
            ConverterErrors.errorsList.Add(Errs[4])
          else
          begin
            startDateList := TempListArr[2];
            endDateList := TempListArr[3];

            swmmSeriesStrtDate := EncodeDate(StrToInt(startDateList[0]),
              StrToInt(startDateList[1]), StrToInt(startDateList[2]));
            swmmSeriesEndDate := EncodeDate(StrToInt(endDateList[0]),
              StrToInt(TempListArr[3][1]), StrToInt(endDateList[2]));

          end;
          lblTSStartEndDate.Caption :=
            Format('Simulation Period: From %s to %s',
            [FormatDateTime('mm/dd/yyyy', swmmSeriesStrtDate),
            FormatDateTime('mm/dd/yyyy', swmmSeriesEndDate)]);
        end
        else // we are exporting to swmm so using SWMM input file
        begin

          // 0-NodeIDs list, 1-Pollutants list, 2-Timeseries list, 3-Inflows list
          swmmFileContents := readSWMMInputFile(swmmFilePath);
          swmmIDsListArr := SWMMIO.getSWMMNodeIDsFromTxtInput(swmmFileContents);
          SWMMIO.TSList := swmmIDsListArr[2];
          SWMMIO.InflowsList := swmmIDsListArr[3];
          SWMMIO.NodeNameList := swmmIDsListArr[0];
          SWMMIO.PollList := swmmIDsListArr[1];

          swmmSeriesStrtDate := StrToDateTime(swmmIDsListArr[4][0]);
          swmmSeriesEndDate := StrToDateTime(swmmIDsListArr[4][1]);
        end
      end
      else
      begin
        { Otherwise, raise an exception. }
        raise Exception.Create('File does not exist.');
        exit
      end;

      // check dates in swmm file again dates from groupnames.txt
      // if swmm timeseries starts later than FW timespan set start datepicker to swmm timeseries start
      if ((strtDatePicker.Date < swmmSeriesStrtDate) or (strtDatePicker.Date > swmmSeriesEndDate)) then
      begin
        strtDatePicker.Date := swmmSeriesStrtDate;
        InputGroupNames.startDate := swmmSeriesStrtDate;
      end;

      // if swmm timeseries ends earlier than FW timespan set end datepicker to swmm timeseries end
      if ((endDatePicker.Date > swmmSeriesEndDate) or (endDatePicker.Date < swmmSeriesStrtDate))then
      begin
        endDatePicker.Date := swmmSeriesEndDate;
        InputGroupNames.endDate := swmmSeriesEndDate;
      end;

      lbxAvailSWMMNodes.Items := SWMMIO.NodeNameList;
      lbxAvailSWMMConstituents.Items := SWMMIO.PollList;
      SWMMIO.PollList.Insert(0, 'Exclude');
    end;
  finally
  end;
end;

procedure TForm1.endDatePickerChange(Sender: TObject);
begin
  if (endDatePicker.Date > swmmSeriesEndDate) then
  begin
    MessageDlg
      ('The end date you selected is later than the time span of the available SWMM timeseries, please try again',
      mtInformation, [mbOK], 0);
    endDatePicker.Date := swmmSeriesEndDate;
  end;
  InputGroupNames.endDate := endDatePicker.Date;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (assigned(endDateList)) then
    endDateList.Free;
  if (assigned(ConverterErrors.errorsList)) then
    ConverterErrors.errorsList.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  numConstituents, I: integer;

begin
  Height := 130;
  Form1.color := clwhite;

  // 0. For SWMM_TO_FW, if groupnames file does not exist cannot continue, alert user and exit else get FW time span
  // If (SWMMIO.operatingMode = SWMMIO.opModes[0]) then
  // begin
  If (ConverterErrors.checkInputFiles() = -1) then
  begin
    MessageDlg
      ('A Valid Framework File (groupnames.txt) was not found. SWMM Converter cannot continue',
      mtInformation, [mbOK], 0);
    Self.Close();
  end
  else
  begin
    // read groupnames file, extract dates and list of files for use later and set time span datepickers
    InputGroupNames := FWIO.readGroupNames();
    strtDatePicker.DateTime := InputGroupNames.startDate;
    endDatePicker.DateTime := InputGroupNames.endDate;
  end;
  // end;

  lblTSStartEndDate.Caption := '';
  if (SWMMIO.operatingMode = SWMMIO.opModes[0]) then
  // SWMM_TO_FW importing from swmm binary file
  begin
    lblOperatingMode.Caption := 'Importing to WERF Framwork from SWMM';
    btnSelectSWMMFile.Caption := 'Select SWMM Results Output File';
    OpenTextFileDialog1.Filter := 'SWMM Results (*.out)|*.OUT';
    SaveTextFileDialog1.Filter := 'SWMM Input (*.inp)|*.INP';
    lblSelectedFWConstituents.Caption := 'Selected For Import to Framework';
  end
  else // SWMM_FROM_FW we are exporting to swmm so using SWMM input file
  begin
    lblOperatingMode.Caption := 'Exporting from WERF Framework to SWMM';
    btnSelectSWMMFile.Caption := 'Select SWMM Input File';
    OpenTextFileDialog1.Filter := 'SWMM Input (*.inp)|*.INP';
    SaveTextFileDialog1.Filter := 'SWMM Input (*.inp)|*.INP';
    lblSelectedFWConstituents.Caption := 'Selected For Export from Framework';
    strtDatePicker.Hide; // not needed for SWMM_FROM_FW
    endDatePicker.Hide; // not needed for SWMM_FROM_FW
    lblTimeSpanTitle.Hide; // not needed for SWMM_FROM_FW
    lblStrtDatePicker.Hide; // not needed for SWMM_FROM_FW
    lblEndDatePicker.Hide; // not needed for SWMM_FROM_FW
    lblTimeSpanTitleNo.Hide; // not needed for SWMM_FROM_FW
  end;

  // prepare framework to SWMM pollutant matching grid to be populated
  numConstituents := Length(SWMMIO.constituentNames);

  // clear all list boxes
  lbxAvailSWMMNodes.Items.Clear;
  lbxSelectedSWMMNodes.Items.Clear;
  lbxAvailSWMMConstituents.Items.Clear;
  lbxSelectedSWMMConstituents.Items.Clear;

  // default constituents
  for I := 0 to numConstituents - 1 do
  begin
    // pollutant framework pollutants list box with framework pollutant names
    lbxAvailSWMMConstituents.Items.Add(constituentNames[I]);
  end;

  // remove flow from the fw constituents list box since always required
  lbxAvailSWMMConstituents.Items.Delete(0);
end;

end.
