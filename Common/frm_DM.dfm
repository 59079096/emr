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
  object qryTemp: TFDQuery
    Connection = conn
    Left = 24
    Top = 88
  end
  object fdphysqltdrvrlnk1: TFDPhysSQLiteDriverLink
    Left = 104
    Top = 24
  end
  object fdgxwtcrsr1: TFDGUIxWaitCursor
    Provider = 'FMX'
    Left = 208
    Top = 24
  end
end
