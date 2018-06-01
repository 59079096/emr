object dm: Tdm
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 448
  Width = 420
  object conn: TFDConnection
    Params.Strings = (
      'DriverID=SQLite')
    Left = 24
    Top = 24
  end
  object fdgxwtcrsr: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 176
    Top = 24
  end
  object fdphysqltdrvrlnk: TFDPhysSQLiteDriverLink
    Left = 96
    Top = 24
  end
  object qryTemp: TFDQuery
    Connection = conn
    Left = 24
    Top = 80
  end
end
