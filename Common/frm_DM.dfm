object dm: Tdm
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 150
  Width = 215
  object conn: TFDConnection
    Params.Strings = (
      'DriverID=SQLite')
    Left = 14
    Top = 12
  end
  object fdgxwtcrsr: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 155
    Top = 12
  end
  object fdphysqltdrvrlnk: TFDPhysSQLiteDriverLink
    Left = 75
    Top = 12
  end
  object qryTemp: TFDQuery
    Connection = conn
    Left = 14
    Top = 68
  end
end
