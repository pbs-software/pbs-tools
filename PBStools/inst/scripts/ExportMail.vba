Attribute VB_Name = "ExportMail"
Sub ExportMailFromOutlook()
    
  ' Adapted from code by Ronnie Munoz
  ' https://www.experts-exchange.com/questions/28025044/VBA-in-Access-2010-to-import-emails-located-in-Outlook-Public-Sub-Folders-Including-Folder-Name-Attachments.html
  ' Code for specifing top level folder and initializing routine.
    
  Dim ns As Outlook.NameSpace
  Dim objFolder As Outlook.MAPIFolder
  Dim objSubFolder As Outlook.MAPIFolder
  Dim objItems As Outlook.Items

  Set ns = GetNamespace("MAPI")
  Set objFolder = ns.PickFolder

  'Set info and call GetMailProp code.
  Set objItems = objFolder.Items
  GetMailProp objItems, objFolder
    
  'Set info and call ProcessSubFolders.
  For Each objSubFolder In objFolder.Folders
    Set objItems = objSubFolder.Items
    ProcessSubFolders objItems, objSubFolder
  Next
  
End Sub

Sub GetMailProp(objProp As Outlook.Items, objFolderProp As Outlook.MAPIFolder)

  ' Code for writing Outlook mail properties to Access.
  etable = "Biblio"
  Dim adoConn As Object
  Dim adoRS As Object
  Dim tmpRS As Object
  Dim intRows As Integer
  Dim EID As String
   
  Set adoConn = CreateObject("ADODB.Connection")
  adoConn.Provider = "Microsoft.Jet.OLEDB.4.0;Data Source=C:/Users/haighr/Files/Archive/Mail/Email.mdb"
  adoConn.Open
  Set adoRS = CreateObject("ADODB.RecordSet")
  adoRS.Open "Select * from " & etable, adoConn, 2, 3

  'Write Outlook mail properties to Access "Email" table.
  iNumMessages = objProp.Count
  If iNumMessages <> 0 Then
    For i = 1 To iNumMessages
      If TypeName(objProp(i)) = "MailItem" Then
        Set cMail = objProp(i)
        EID = cMail.EntryID
        Set tmpRS = CreateObject("ADODB.RecordSet")
        tmpRS.Open "Select Count(*) as Nrec From " & etable & " where [EntryID] = '" & EID & "'", adoConn, 3, 3  ' Need to use a static cursor
        intRows = tmpRS.Fields("Nrec")
        tmpRS.Close
        If intRows = 0 Then
          'Code used to insert individual outlook mail properties. Properties must match fields in Access table titled "Email."
          adoRS.AddNew
          adoRS("Folder") = objFolderProp.Name
          adoRS("Received") = cMail.SentOn
          'adoRS("Subject") = cMail.Subject
          If cMail.Subject = "" Then
            adoRS("Subject") = "Message from " & cMail.SenderName
            Else: adoRS("Subject") = cMail.Subject
          End If
          'adoRS("From") = cMail.SenderName
          If cMail.SenderName = "" Then
            adoRS("From") = "Unknown"
            Else: adoRS("From") = cMail.SenderName
          End If
          'adoRS("To") = cMail.To
          If cMail.To = "" Then
            adoRS("To") = "Unknown"
            Else: adoRS("To") = cMail.To
          End If
          adoRS("CC") = cMail.CC
          adoRS("BCC") = cMail.BCC
          adoRS("Size") = cMail.Size
          'adoRS("Message") = cMail.Body
          If cMail.Body = "" Then
            adoRS("Message") = cMail.Subject
            Else: adoRS("Message") = cMail.Body
          End If
          adoRS("Path") = objFolderProp.FolderPath
          adoRS("ConversationID") = cMail.ConversationID
          adoRS("EntryID") = cMail.EntryID
          adoRS.Update
        End If
      End If
    Next i
  End If
  adoRS.Close
  adoConn.Close
End Sub


Sub ProcessSubFolders(objItemsR As Outlook.Items, objFolderR As Outlook.MAPIFolder)
    
  'Code for processing subfolders. Code is recursive so all subfolders are processed.
    
  ' Set up Outlook objects.
  Dim objSubFolderR As Outlook.MAPIFolder

  'Set info and call GetMailProp code.
  GetMailProp objItemsR, objFolderR
    
  'Set info and call ProcessSubFolders. Recursive.
  For Each objSubFolderR In objFolderR.Folders
    Set objItemsR = objSubFolderR.Items
    ProcessSubFolders objItemsR, objSubFolderR
  Next

End Sub



