{ ------------------------------------------------------------------- }
{ Unit:    SWMMDrivers.pas }
{ Project: WERF Framework - SWMM Converter }
{ Version: 2.0 }
{ Date:    2/28/2014 }
{ Author:  Gesoyntec (D. Pankani) }
{ }
{ Delphi Pascal unit that for the main interface GUI that }
{ ------------------------------------------------------------------- }
unit SWMMDrivers;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Generics.Defaults, Generics.Collections,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtDlgs, Vcl.Grids,
  Vcl.ExtCtrls, UserInputConfirmationDlg, OperationStatusDlgFrm,
  ImportHelpDialogFrm, ExportHelpDlgFrm, SWMMIO,
  SWMMInput, SWMMOutput, ReadMTA, WriteMTA, ComCtrls, BusyDialogFrm, GIFImg,
  FWControlScratchFile, StrUtils, GSControlGrid;

const

  Errs: array [0 .. 1] of string =
    ('An unknown error occured when reading the SWMM file',
    'An unknown error occured when saving the new SWMM file');

type
  TForm1 = class(TForm)
    OpenTextFileDialog1: TOpenTextFileDialog;
    SaveTextFileDialog1: TSaveTextFileDialog;
    btnSelectSWMMFile: TButton;
    txtSwmmFilePath: TLabel;
    cbxSwmmNode: TComboBox;
    sgdUserInputGrid: TStringGrid;
    Label2: TLabel;
    btnCancel: TButton;
    btnHelp: TButton;
    btnNext: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    RadioGroup1: TRadioGroup;
    txtScenarioDescr: TEdit;
    Label1: TLabel;
    Label9: TLabel;
    lblOperatingMode: TLabel;
    lblHelp: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    lbxFWConstituents: TListBox;
    lbxSWMMConstituents: TListBox;
    btnLinkConstituents: TButton;
    lbxLinkedConstituents: TListBox;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    unlinkConstituents: TButton;

    /// <summary>
    /// Handler for button used to browse to SWMM file
    /// </summary>
    /// <param name="Sender">
    /// Owner of the button (this form)
    /// </param>
    procedure btnSelectSWMMFileClick(Sender: TObject);

    /// <summary>
    /// Handler for form show event. Most of the setup of the form occurs in
    /// this method
    /// </summary>
    /// <param name="Sender">
    /// Owner (this form)
    /// </param>
    procedure FormShow(Sender: TObject);

    /// <summary>
    /// Displays a dialog to the user for confirmation of user input
    /// parameters and then processes the user import or export request when
    /// the confirmation dialog is dismissed
    /// </summary>
    /// <param name="Sender">
    /// Parent form - currently not used
    /// </param>
    procedure btnNextClick(Sender: TObject);
    // procedure ProgressCallback(InProgressOverall: TProgressBar);
    // procedure RadioGroup1Click(Sender: TObject);

    /// <summary>
    /// Handler for cancel button
    /// </summary>
    /// <param name="Sender">
    /// Owner of the button (this form)
    /// </param>
    procedure btnCancelClick(Sender: TObject);

    /// <summary>
    /// Change handler for SWMM node selection combo box
    /// </summary>
    /// <param name="Sender">
    /// Owner of the combo box (this form)
    /// </param>
    procedure cbxSwmmNodeChange(Sender: TObject);

    /// <summary>
    /// Handler for help button. Displays a dialog with help on how to use
    /// this tool
    /// </summary>
    /// <param name="Sender">
    /// Owner of the button (this form)
    /// </param>
    procedure btnHelpClick(Sender: TObject);

    /// <summary>
    /// Handler for help link. Displays a dialog with help on how to use this
    /// tool
    /// </summary>
    /// <param name="Sender">
    /// Owner of the button (this form)
    /// </param>
    procedure lblHelpClick(Sender: TObject);
    procedure btnLinkConstituentsClick(Sender: TObject);
    procedure unlinkConstituentsClick(Sender: TObject);

  private
    workingDirPath: string;

    /// <summary>
    /// method for populating a TMTARecFields data structure which holds user input
    /// collected on this form
    /// </summary>
    /// <param name="SWMMNodeName">
    /// Name of SWMM node being acted on
    /// </param>
    /// <param name="Conv">
    /// Name of SWMM node being acted on
    /// </param>
    /// <param name="constituentName">
    /// Name of constituent in Framework
    /// <param name="constituentName">
    /// Name of constituent in SWMM
    /// </param>
    /// <param name="convFactor">
    /// Conversion factor
    /// </param>
    /// <param name="tsType">
    /// Type of constituent (either CONCEN or FLOW)
    /// </param>
    /// <param name="ScenarioDescription">
    /// Description of scenario
    /// </param>
    /// <param name="swmmFilePath">
    /// SWMM input or output filepath
    /// </param>
    /// <param name="fwScratchFilePath">
    /// Framework control scratch file path
    /// </param>
    /// <param name="mtaFilePath">
    /// SWMM control file path
    /// </param>
    procedure AssignMTARecFields(SWMMNodeName: string; var Conv: TMTARecord;
      constituentName: string; constituentSWMMName: string; convFactor: Double;
      tsType: string; ScenarioDescription: string; swmmFilePath: string;
      fwScratchFilePath: string; mtaFilePath: string);
    { Private declarations }

  var
    swmmFilePath: string;

  public
    { Public declarations }

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnCancelClick(Sender: TObject);
begin
  Self.Close;
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

procedure TForm1.btnNextClick(Sender: TObject);
var
  ConvertedFWTSArr: TArray<TMTARecord>;
  //constituentCbxs: TArray<TComboBox>;
  tempStr: string;
  j: Integer;
  tempModalResult: Integer;
  numSelectedConstituents: Integer;
  newSWMMInputFilePath: string;
  selectedNodeName: string;
  scenarioDescr: string;
  FWCtrlRecord: FWCtrlScratchRecord;
  fwTSFileToCreatePath: string;
  TempStrList: TStringList;
begin

  // Launch project form and force user to provide the project information
  SWMMUserInputVerificationFrm := TUserInputVerificationFrm.Create(Application);

  SetLength(ConvertedFWTSArr, High(constituentNames) + 1);
  scenarioDescr := txtScenarioDescr.Text;
  if (operatingMode = opModes[0]) then // importing from swmm Binary
    newSWMMInputFilePath := Self.txtSwmmFilePath.Caption
  else // exporting to swmm input file
  begin
    // the modified swmm file will be saved as new swmm file with a datetime suffix at the same location
    // as the input swmm file into which the framework ts will be exported
    newSWMMInputFilePath := workingDirPath + '\' +
      AnsiLeftStr(ChangeFileExt(ExtractFileName(Self.txtSwmmFilePath.Caption),
      ''), 10) + '_' + FormatDateTime('yyyymmddhhnnss', Now) + '.inp';
  end;

  with SWMMUserInputVerificationFrm do
  begin
    try
      begin // populate dialog fields
        txtSwmmFilePath.Caption := Self.txtSwmmFilePath.Caption;

        // get the name of the selected SWMM node
        if Self.cbxSwmmNode.ItemIndex <> -1 then
          txtSWMMNodeID.Caption := cbxSwmmNode.Items[cbxSwmmNode.ItemIndex];
        selectedNodeName := txtSWMMNodeID.Caption;

        // make up file path for the Framework scratch timeseries data file that will be created
        // this is used only when importing into framework from swmm
        fwTSFileToCreatePath := ExtractFilePath(swmmFilePath) + selectedNodeName
          + 'FWTS.sct';

        // make up file path for the Converter Control file .mta used in both import/export
        SWMMIO.mtaFilePath := ExtractFilePath(swmmFilePath) + selectedNodeName +
          SWMMIO.operatingMode + '.mta';

        // column headers of grid on user confirmation dialog
        StringGrid1.Cells[0, 0] := '* Framework Constituent';
        StringGrid1.Cells[1, 0] := 'SWMM Equivalent';
        StringGrid1.Cells[2, 0] := 'Unit Conversion Factor';

        // Fill in confirmation string grid with user inputs
        for j := Low(constituentNames) to High(constituentNames) do
        begin
          StringGrid1.Cells[0, j + 1] := constituentNames[j];
          // set all cells to 'not selected' and overwrite selected ones later
          StringGrid1.Cells[1, j + 1] := 'Not Selected';
          StringGrid1.Cells[2, j + 1] := Self.sgdUserInputGrid.Cells[2, j + 1];
        end;

        // Flow is a special case constituent which is always selected
        StringGrid1.Cells[1, 1] := 'FLOW'; // flow is always selected
        AssignMTARecFields(selectedNodeName, ConvertedFWTSArr[0],
          constituentNames[0], 'FLOW', StrToFloat(Self.sgdUserInputGrid.Cells[2,
          1]), 'FLOW', scenarioDescr, newSWMMInputFilePath,
          fwTSFileToCreatePath, mtaFilePath);

        // save conversion factors and selected constituents into record array for use later
        numSelectedConstituents := 1;
        for j := Low(constituentNames) + 1 to High(constituentNames) do
        begin
          if (sgdUserInputGrid.Cells[1, j + 1] <> '') then
          begin
          //copy swmm constituent name to confirmation grid
            StringGrid1.Cells[1, j + 1] := sgdUserInputGrid.Cells[1, j + 1];

            //populate records with results of match and conversion factors
            AssignMTARecFields(selectedNodeName, ConvertedFWTSArr[j],
              constituentNames[j], sgdUserInputGrid.Cells[1, j + 1],
              StrToFloat(Self.sgdUserInputGrid.Cells[2, j + 1]), 'CONCEN',
              scenarioDescr, newSWMMInputFilePath, fwTSFileToCreatePath,
              mtaFilePath);
            inc(numSelectedConstituents);
          end;
        end;
      end;
      lblNumberOfConstituents.Caption := 'Constituents (' +
        IntToStr(numSelectedConstituents) + ' of 8 selected)';

        //show the user confirmation dialog
      tempModalResult := SWMMUserInputVerificationFrm.ShowModal;
      if (tempModalResult = mrOk) then
      begin
        Application.CreateForm(TBusyFrm, BusyFrm);
        Self.Hide;

        Screen.FocusedForm := BusyFrm;
        SendMessage(BusyFrm.Handle, CM_ACTIVATE, 0, 0);
        (BusyFrm.Image1.Picture.Graphic as TGIFImage).AnimateLoop := glEnabled;
        (BusyFrm.Image1.Picture.Graphic as TGIFImage).Animate := true;
        BusyFrm.Show;
        Application.ProcessMessages;

        if (operatingMode = opModes[0]) then // importing from swmm Binary
        begin
          BusyFrm.Caption := 'Importing from SWMM';

          //call module to do the work of importing into framework from swmm
          FWCtrlRecord := SWMMOutput.importFromSWMMToFW(swmmFilePath,
            fwTSFileToCreatePath, selectedNodeName, ConvertedFWTSArr);

          // save user settings to MTA file to allow subsequent commandline executions without user intervention
          WriteMTA.Write(mtaFilePath, ConvertedFWTSArr);
          BusyFrm.Close;

          // populate operation status confirmation dialog
          OperationStatusDlg.Caption := 'Import Operation Status';
          OperationStatusDlg.lblHeader.Caption :=
            'Import Operation was successful!';
          OperationStatusDlg.lblSWMMFilePath.Visible := false;
          OperationStatusDlg.lblSWMMFileLabel.Visible := false;
          OperationStatusDlg.lblTSFilesCreated.Caption :=
            FWCtrlRecord.scratchFilePath;
          OperationStatusDlg.ShowModal;
        end
        else // exporting to swmm input file
        begin
          BusyFrm.Caption := 'Exporting to SWMM';

          // open framework control file previously created by framework
          //and read in path to scratchfile that points to the timeseries data file
          TempStrList := TStringList.Create();
          TempStrList.LoadFromFile(SWMMIO.frameCtrlFilePath);

          // first line in the framework control file should be the path to the timeseries data file
          //read in framework timeseries data file and convert it into an array of swmm timeseries files
          ConvertedFWTSArr := SWMMIO.readInFrameworkTSFile(TempStrList[0],
            ConvertedFWTSArr);

          //save converted timeseries files to disc
          SWMMInput.finalizeExport(ConvertedFWTSArr, workingDirPath);

          // write the list of newly exported swmm timeseries file paths to the confirmation dialog
          //for the use to see
          for j := Low(ConvertedFWTSArr) to High(ConvertedFWTSArr) do
          begin
            if (ConvertedFWTSArr[j].convertedTSFilePath <> '') then
              tempStr := tempStr + #13#10 + ConvertedFWTSArr[j]
                .convertedTSFilePath;
          end;

          // write TS and Inflow blocks to swmm input file and save as new file
          SWMMInput.updateSWMMInputFile(ConvertedFWTSArr, swmmFilePath,
            newSWMMInputFilePath);

          // save user settings to MTA file to allow subsequent commandline executions without user intervention
          WriteMTA.Write(mtaFilePath, ConvertedFWTSArr);
          BusyFrm.Close;

          // populate operation status confirmation dialog
          OperationStatusDlg.Caption := 'Export Operation Status';
          OperationStatusDlg.lblHeader.Caption :=
            'Export Operation was successful!';
          OperationStatusDlg.lblSWMMFilePath.Caption := newSWMMInputFilePath;
          OperationStatusDlg.lblTSFilesCreated.Caption := tempStr;
          OperationStatusDlg.ShowModal;
          Self.Close;
        end;
        Self.Close;
      end;
    finally
      BusyFrm.Close;
      // ConvertedFWTSArr.Free;
    end;
  end;
end;

procedure TForm1.AssignMTARecFields(SWMMNodeName: string; var Conv: TMTARecord;
  constituentName: string; constituentSWMMName: string; convFactor: Double;
  tsType: string; ScenarioDescription: string; swmmFilePath: string;
  fwScratchFilePath: string; mtaFilePath: string);
begin
  Conv.tsUnitsFactor := 1.0;
  Conv.constituentSWMMName := constituentSWMMName;
  Conv.constituentFWName := constituentName;
  Conv.convFactor := convFactor;
  Conv.tsNodeName := SWMMNodeName;
  Conv.tsType := tsType;
  Conv.tsName := constituentName;
  Conv.modelRunScenarioID := ScenarioDescription;
  Conv.swmmFilePath := swmmFilePath;
  Conv.scratchFilePath := fwScratchFilePath;
  Conv.mtaFilePath := mtaFilePath;
end;

procedure TForm1.btnSelectSWMMFileClick(Sender: TObject);
var
  TempListArr: TArray<TStringList>;
  // intTokenLoc: Integer;
  swmmIDsListArr: TArray<TStringList>;
begin
  // intTokenLoc := 0;
  try
    if OpenTextFileDialog1.Execute then
    begin
      { First check if the file exists. }
      if FileExists(OpenTextFileDialog1.FileName) then
      begin
        Height := 234;
        // save the directory so can write TS to same directory later
        workingDirPath := ExtractFileDir(OpenTextFileDialog1.FileName);
        swmmFilePath := OpenTextFileDialog1.FileName;
        txtSwmmFilePath.Caption := swmmFilePath;

        if (SWMMIO.operatingMode = SWMMIO.opModes[0]) then
        // importing from swmm binary file
        begin
          TempListArr := SWMMIO.getSWMMNodeIDsFromBinary(swmmFilePath);
          if (TempListArr[0].Count < 1) then
            SWMMIO.errorsList.Add(Errs[0]);
          // Unable to read nodes in SWMM input file
          if (TempListArr[1].Count < 1) then
            SWMMIO.errorsList.Add(Errs[0]);
          // Unable to read pollutant names in SWMM input file

          NodeNameList := TempListArr[0];
          PollList := TempListArr[1];
        end
        else // we are exporting to swmm so using SWMM input file
        begin

          // 0-NodeIDs list, 1-Pollutants list, 2-Timeseries list, 3-Inflows list
          swmmIDsListArr := SWMMIO.getSWMMNodeIDsFromTxtInput(swmmFilePath);
          SWMMIO.TSList := swmmIDsListArr[2];
          SWMMIO.InflowsList := swmmIDsListArr[3];
          SWMMIO.NodeNameList := swmmIDsListArr[0];
          SWMMIO.PollList := swmmIDsListArr[1];
        end
      end
      else
      begin
        { Otherwise, raise an exception. }
        raise Exception.Create('File does not exist.');
        exit
      end;
      cbxSwmmNode.Items := SWMMIO.NodeNameList;

      lbxSWMMConstituents.Items := SWMMIO.PollList;
      // TODO delete left over from when using comboboxes for matching
      SWMMIO.PollList.Insert(0, 'Exclude');
      { cbxFlow.Items := SWMMIO.PollList;
        cbxDCu.Items := SWMMIO.PollList;
        cbxTCu.Items := SWMMIO.PollList;
        cbxTZn.Items := SWMMIO.PollList;
        cbxDZn.Items := SWMMIO.PollList;
        cbxDP.Items := SWMMIO.PollList;
        cbxTP.Items := SWMMIO.PollList;
        cbxTSS.Items := SWMMIO.PollList; }
    end;
  finally
  end;
end;

procedure TForm1.btnLinkConstituentsClick(Sender: TObject);
var
  I: Integer;
  constituentName: String;
begin
  if ((lbxFWConstituents.ItemIndex <> -1) and
    (lbxSWMMConstituents.ItemIndex <> -1)) then
  begin
    constituentName := lbxFWConstituents.Items.Strings
      [lbxFWConstituents.ItemIndex];
    // added linked constituents to linked constituents list box
    lbxLinkedConstituents.Items.Add(constituentName + ' <=> ' +
      lbxSWMMConstituents.Items.Strings[lbxSWMMConstituents.ItemIndex]);

    // update the string grid with the matching
    for I := 1 to sgdUserInputGrid.RowCount do
    begin
      // find framework constituent and write corresponding swmm constituent
      if (constituentName = sgdUserInputGrid.Cells[0, I]) then
      begin
        sgdUserInputGrid.Cells[1, I] := lbxSWMMConstituents.Items.Strings
          [lbxSWMMConstituents.ItemIndex];
        // exit;
      end;
    end;
    // removed constituents from their source list boxes
    lbxFWConstituents.Items.Delete(lbxFWConstituents.ItemIndex);
    lbxSWMMConstituents.Items.Delete(lbxSWMMConstituents.ItemIndex);

  end;
end;

procedure TForm1.unlinkConstituentsClick(Sender: TObject);
var
  I: Integer;
  TempStrList: TStringList;
begin
  if (lbxLinkedConstituents.ItemIndex <> -1) then
  begin
    // create a string list to hold split strings to be put back in fw and swmm constituent list boxes
    TempStrList := TStringList.Create();

    // split the string selected in the matched constituents list box
    SWMMIO.Split(' ', lbxLinkedConstituents.Items.Strings
      [lbxLinkedConstituents.ItemIndex], TempStrList);

    // add the first string in the stringlist to the fw constituents list box and the second to the swmm one
    lbxFWConstituents.Items.Add(TempStrList[0]);
    lbxSWMMConstituents.Items.Add(TempStrList[2]);

    // update the string grid and removing the matching
    for I := 1 to sgdUserInputGrid.RowCount do
    begin
      // find framework constituent delete it
      if (TempStrList[0] = sgdUserInputGrid.Cells[0, I]) then
      begin
        sgdUserInputGrid.Cells[1, I] := '';
        // exit;
      end;
    end;
    // removed the matching from the matched constituents list box
    lbxLinkedConstituents.Items.Delete(lbxLinkedConstituents.ItemIndex);
    TempStrList.Free();
  end;
end;

procedure TForm1.cbxSwmmNodeChange(Sender: TObject);
begin
  // Height := 620;
  Height := 800;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  numConstituents, I: Integer;
begin
  Height := 130;
  SWMMIO.errorsList := TStringList.Create();
  Form1.color := clwhite;

  // need framwork metadata scratchfile file for export
  // control file tells us what the name of the framework node is that we will be exporting from
  // so if framwork metadata scratchfile was not passed in via commandline so ask for it
  if (SWMMIO.frameCtrlFilePath = '') and
    (SWMMIO.operatingMode = SWMMIO.opModes[1]) then
  begin
    SWMMIO.frameCtrlFilePath := SWMMIO.workingDir + 'swmmconvertstring.txt';
    if (Not(FileExists(SWMMIO.frameCtrlFilePath))) then
    begin
      MessageDlg
        ('A Valid Framework Control File was not located. Press okay to browse and select one',
        mtInformation, [mbOK], 0);
      OpenTextFileDialog1.Filter := 'Framework Control File (*.txt)|*.TXT';

      if OpenTextFileDialog1.Execute then
      begin
        { First check if the file exists. }
        if FileExists(OpenTextFileDialog1.FileName) then
        begin
          SWMMIO.frameCtrlFilePath := OpenTextFileDialog1.FileName;
        end
        else // we are exporting to swmm so using SWMM input file
        begin
          MessageDlg('A Valid Framework Control File was not Found',
            mtInformation, [mbOK], 0);
        end;
      end;
    end;
  end;

  if (SWMMIO.operatingMode = SWMMIO.opModes[0]) then
  // SWMM_TO_FW importing from swmm binary file
  begin
    lblOperatingMode.Caption := 'Importing from SWMM to WERF Framework';
    btnSelectSWMMFile.Caption := 'Select SWMM Results Output File';
    OpenTextFileDialog1.Filter := 'SWMM Results (*.out)|*.OUT';
    SaveTextFileDialog1.Filter := 'SWMM Input (*.inp)|*.INP';
  end
  else // SWMM_FROM_FW we are exporting to swmm so using SWMM input file
  begin
    lblOperatingMode.Caption := 'Exporting from WERF Framework to SWMM';
    btnSelectSWMMFile.Caption := 'Select SWMM Input File';
    OpenTextFileDialog1.Filter := 'SWMM Input (*.inp)|*.INP';
    SaveTextFileDialog1.Filter := 'SWMM Input (*.inp)|*.INP';
  end;

  // prepare framework to SWMM pollutant matching grid to be populated
  numConstituents := Length(SWMMIO.constituentNames);
  sgdUserInputGrid.RowCount := numConstituents + 1;
  // column headers
  sgdUserInputGrid.Cells[0, 0] := '* Framework Constituent';
  sgdUserInputGrid.Cells[1, 0] := 'SWMM Equivalent';
  sgdUserInputGrid.Cells[2, 0] := 'Unit Conversion Factor';

  lbxFWConstituents.Items.Clear;
  // default constituents
  for I := 0 to numConstituents - 1 do
  begin
    // pollutant framework pollutants list box with framework pollutant names
    lbxFWConstituents.Items.Add(constituentNames[I]);

    sgdUserInputGrid.Cells[0, I + 1] := constituentNames[I];
    sgdUserInputGrid.Cells[2, I + 1] := '1.00';
  end;

  // remove flow from the fw constituents list box since always required
  lbxFWConstituents.Items.Delete(0);

  // default selected constituents - Flow is allways selected
  sgdUserInputGrid.Cells[1, 1] := 'FLOW ';
end;

end.
