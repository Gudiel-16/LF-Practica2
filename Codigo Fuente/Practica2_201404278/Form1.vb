Imports System.IO
Imports System.Drawing
Public Class Form1

    Dim contador_token As Integer = 0
    Dim contador_error_1 As Integer = 0
    Dim contador_error_2 As Integer = 0
    Dim corchete_que_abre As Integer = 0
    Dim corchete_que_cierra As Integer = 0

    Private Sub bttAnalizar_Click(sender As Object, e As EventArgs) Handles bttAnalizar.Click
        corchete_que_abre = 0
        corchete_que_cierra = 0

        metodoLimpiarTodo()

        If contadorcorchetes() = 4 And contadorcorchetes2() = 4 Then
            inicioAnalizar()
        Else
            contador_error_2 = contador_error_2 + 1
            dgvErrores2.Rows.Add(contador_error_2, "ERROR SINTACTICO: -> Corchetes ']' '[' ", "--", "--")

        End If


    End Sub


    Public Sub metodo_analizar()
        Dim contadorabrirEstados As Integer = 0 ' para saber cuando cambiar de estado, ira entre los [ ] es cuando abre
        Dim contadorcerrarEstados As Integer = 0 ' para saber cuando cambiar de estado, ira entre los [ ] cuando cierra
        Dim caracterAUX_1 As String = ""
        Dim caracterAUX_2 As String = ""
        Dim caracter_1 As String = ""
        Dim columna_1 As Integer = 1
        Dim fila_1 As Integer = 1
        Dim examinar_ascii_1 As Integer = 0
        Dim estado As Integer = 100
        Dim examinar_1 As String = ""
        Dim texto_1 As String = areaAnalizar.Text.ToCharArray 'me sapara todos los caracter del area analizar para que vaya analizando caracter por caracter

        For puntero_1 As Integer = 0 To areaAnalizar.TextLength - 1 'ciclo para que vaya anilizando cada caracter y me de el codigo ascii
            examinar_1 = texto_1(puntero_1)
            examinar_ascii_1 = Asc(examinar_1)
            'Console.WriteLine(examinar_ascii_1)

            Select Case estado

                Case 100
                    If examinar_ascii_1 = 10 Then  'salto de linea'
                        fila_1 = fila_1 + 1
                        columna_1 = 1
                        estado = 100
                    ElseIf examinar_ascii_1 = 32 Then   'espacio en blanco'
                        columna_1 = columna_1 + 1
                        estado = 100
                    ElseIf examinar_ascii_1 = 9 Then 'tabulacion'
                        columna_1 = columna_1 + 1
                        estado = 100
                    ElseIf (examinar_ascii_1 > 64 And examinar_ascii_1 < 91) Or (examinar_ascii_1 > 96 And examinar_ascii_1 < 123) Then 'letras'
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 101
                    ElseIf examinar_ascii_1 = 61 Then ' =
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 102
                    ElseIf examinar_ascii_1 = 123 Then ' { 
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 102
                    ElseIf examinar_ascii_1 = 91 Then ' [  
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        contadorabrirEstados = contadorabrirEstados + 1
                        estado = 102
                    Else
                        metodoErrores2(examinar_1, fila_1, columna_1)
                    End If

                Case 101
                    If (examinar_ascii_1 > 64 And examinar_ascii_1 < 91) Or (examinar_ascii_1 > 96 And examinar_ascii_1 < 123) Then 'letras '
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 101
                    Else
                        metodoErrores(caracter_1, fila_1, columna_1) 'llenar campos de envio lexico'
                        caracter_1 = ""
                        puntero_1 = puntero_1 - 1
                        estado = 100
                    End If


                Case 102
                    metodoErrores(caracter_1, fila_1, columna_1) 'llenar campos de envio lexico'
                    caracter_1 = ""
                    puntero_1 = puntero_1 - 1
                    If contadorabrirEstados = 1 Then
                        Console.WriteLine("CIERRE GRAMATICA")
                        estado = 0
                    Else
                        estado = 100
                    End If

                Case 0
                    If examinar_ascii_1 = 10 Then  'salto de linea'
                        fila_1 = fila_1 + 1
                        columna_1 = 1
                        estado = 0
                    ElseIf examinar_ascii_1 = 32 Then   'espacio en blanco'
                        columna_1 = columna_1 + 1
                        estado = 0
                    ElseIf examinar_ascii_1 = 9 Then 'tabulacion'
                        columna_1 = columna_1 + 1
                        estado = 0
                    ElseIf (examinar_ascii_1 > 32 And examinar_ascii_1 < 44) Or (examinar_ascii_1 > 44 And examinar_ascii_1 < 91) Or (examinar_ascii_1 > 93 And examinar_ascii_1 < 255) Or (examinar_ascii_1 = 92) Then 'letras'
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 1
                    
                    ElseIf examinar_ascii_1 = 91 Then ' [  
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        contadorabrirEstados = contadorabrirEstados + 1
                        estado = 2
                    ElseIf examinar_ascii_1 = 93 Then ' ] 
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        contadorcerrarEstados = contadorcerrarEstados + 1
                        estado = 2
                    ElseIf examinar_ascii_1 = 44 Then ' ,
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 2

                    Else
                        metodoErrores2(examinar_1, fila_1, columna_1)

                    End If


                Case 1
                    If (examinar_ascii_1 > 32 And examinar_ascii_1 < 44) Or (examinar_ascii_1 > 44 And examinar_ascii_1 < 91) Or (examinar_ascii_1 > 93 And examinar_ascii_1 < 255) Or (examinar_ascii_1 = 92) Then 'letras'
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 1
                    Else
                        metodoSimbolosTerminales(caracter_1, fila_1, columna_1, caracterAUX_1, caracterAUX_2) 'llenar campos de envio lexico'
                        caracter_1 = ""
                        puntero_1 = puntero_1 - 1
                        estado = 0
                    End If

                Case 2
                    metodoErrores(caracter_1, fila_1, columna_1) 'llenar campos de envio lexico'
                    caracter_1 = ""
                    puntero_1 = puntero_1 - 1
                    If contadorcerrarEstados = 1 Then
                        Console.WriteLine("CIERRE TERMINALES")
                        estado = 3
                    Else
                        estado = 0
                    End If

                Case 3 ' para los NO terminales
                    If examinar_ascii_1 = 10 Then  'salto de linea'
                        fila_1 = fila_1 + 1
                        columna_1 = 1
                        estado = 3
                    ElseIf examinar_ascii_1 = 32 Then   'espacio en blanco'
                        columna_1 = columna_1 + 1
                        estado = 3
                    ElseIf examinar_ascii_1 = 9 Then 'tabulacion'
                        columna_1 = columna_1 + 1
                        estado = 3
                    ElseIf (examinar_ascii_1 > 64 And examinar_ascii_1 < 91) Or (examinar_ascii_1 > 96 And examinar_ascii_1 < 123) Then 'letras'
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 4
                    ElseIf (examinar_ascii_1 > 47 And examinar_ascii_1 < 58) Then 'numeros'
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 4
                    ElseIf examinar_ascii_1 = 95 Then ' _
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 4
                    ElseIf examinar_ascii_1 = 61 Then ' =
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 5
                    ElseIf examinar_ascii_1 = 44 Then ' ,
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 5
                    ElseIf examinar_ascii_1 = 123 Or examinar_ascii_1 = 125 Then ' { }
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 5
                    ElseIf examinar_ascii_1 = 91 Then ' [  
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        contadorabrirEstados = contadorabrirEstados + 1
                        estado = 5
                    ElseIf examinar_ascii_1 = 93 Then ' ] 
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        contadorcerrarEstados = contadorcerrarEstados + 1
                        estado = 5

                    Else
                        metodoErrores2(examinar_1, fila_1, columna_1)

                    End If


                Case 4 '---------------------
                    If (examinar_ascii_1 > 64 And examinar_ascii_1 < 91) Or (examinar_ascii_1 > 96 And examinar_ascii_1 < 123) Then 'letras minuscula'
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 4
                    ElseIf (examinar_ascii_1 > 47 And examinar_ascii_1 < 58) Then  'numeros'
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 4
                    ElseIf examinar_ascii_1 = 95 Then ' _
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 4
                    Else
                        metodoSimbolosNoTerminales(caracter_1, fila_1, columna_1, caracterAUX_1, caracterAUX_2) 'llenar campos de envio lexico'
                        caracter_1 = ""
                        puntero_1 = puntero_1 - 1
                        estado = 3
                    End If

                Case 5
                    metodoErrores(caracter_1, fila_1, columna_1) 'llenar campos de envio lexico'
                    caracter_1 = ""
                    puntero_1 = puntero_1 - 1

                    If contadorcerrarEstados = 2 Then
                        Console.WriteLine("CIERRE NO TERMINALES")
                        estado = 6
                    Else
                        estado = 3
                    End If

                    ' para las trnsiciones
                Case 6
                    If examinar_ascii_1 = 10 Then  'salto de linea'
                        fila_1 = fila_1 + 1
                        columna_1 = 1
                        estado = 6
                    ElseIf examinar_ascii_1 = 32 Then   'espacio en blanco'
                        columna_1 = columna_1 + 1
                        estado = 6
                    ElseIf examinar_ascii_1 = 9 Then 'tabulacion'
                        columna_1 = columna_1 + 1
                        estado = 6
                    ElseIf (examinar_ascii_1 > 32 And examinar_ascii_1 < 44) Or (examinar_ascii_1 > 44 And examinar_ascii_1 < 91) Or (examinar_ascii_1 > 93 And examinar_ascii_1 < 123) Or (examinar_ascii_1 > 125 And examinar_ascii_1 < 255) Or (examinar_ascii_1 = 92) Or (examinar_ascii_1 = 124) Then 'letras'
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 7

                    ElseIf examinar_ascii_1 = 44 Then ' ,
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 8
                    ElseIf examinar_ascii_1 = 123 Or examinar_ascii_1 = 125 Then ' { }
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 8
                    ElseIf examinar_ascii_1 = 91 Then ' [  
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        contadorabrirEstados = contadorabrirEstados + 1
                        estado = 8
                    ElseIf examinar_ascii_1 = 93 Then ' ] 
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        contadorcerrarEstados = contadorcerrarEstados + 1
                        estado = 8

                    Else
                        metodoErrores2(examinar_1, fila_1, columna_1)

                    End If

                Case 7
                    If (examinar_ascii_1 > 32 And examinar_ascii_1 < 44) Or (examinar_ascii_1 > 44 And examinar_ascii_1 < 91) Or (examinar_ascii_1 > 93 And examinar_ascii_1 < 123) Or (examinar_ascii_1 > 125 And examinar_ascii_1 < 255) Or (examinar_ascii_1 = 92) Or (examinar_ascii_1 = 124) Then 'letras'
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 7
                    ElseIf examinar_ascii_1 = 44 Then ' ,
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 7
                    ElseIf examinar_ascii_1 = 10 Then  'salto de linea'
                        fila_1 = fila_1 + 1
                        columna_1 = 1
                        estado = 7
                    ElseIf examinar_ascii_1 = 32 Then   'espacio en blanco'
                        columna_1 = columna_1 + 1
                        estado = 7
                    ElseIf examinar_ascii_1 = 9 Then 'tabulacion'
                        columna_1 = columna_1 + 1
                        estado = 7

                    Else
                        metodoTransiciones(caracter_1, fila_1, columna_1, caracterAUX_1, caracterAUX_2) 'llenar campos de envio lexico'
                        caracter_1 = ""
                        puntero_1 = puntero_1 - 1
                        estado = 6
                    End If

                Case 8
                    metodoErrores(caracter_1, fila_1, columna_1) 'llenar campos de envio lexico'
                    caracter_1 = ""
                    puntero_1 = puntero_1 - 1

                    If contadorcerrarEstados = 3 Then
                        Console.WriteLine("CIERRE DE TRANSICIONES")
                        estado = 9
                    Else
                        estado = 6
                    End If

                Case 9
                    If examinar_ascii_1 = 10 Then  'salto de linea'
                        fila_1 = fila_1 + 1
                        columna_1 = 1
                        estado = 9
                    ElseIf examinar_ascii_1 = 32 Then   'espacio en blanco'
                        columna_1 = columna_1 + 1
                        estado = 9
                    ElseIf examinar_ascii_1 = 9 Then 'tabulacion'
                        columna_1 = columna_1 + 1
                        estado = 9
                    ElseIf (examinar_ascii_1 > 64 And examinar_ascii_1 < 91) Or (examinar_ascii_1 > 96 And examinar_ascii_1 < 123) Then 'letras'
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 10
                    ElseIf (examinar_ascii_1 > 47 And examinar_ascii_1 < 58) Then 'numeros'
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 10
                    ElseIf examinar_ascii_1 = 95 Then ' _
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 10
                    ElseIf examinar_ascii_1 = 44 Then ' ,
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 11
                    ElseIf examinar_ascii_1 = 123 Or examinar_ascii_1 = 125 Then ' { }
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 11
                    ElseIf examinar_ascii_1 = 91 Then ' [  
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        contadorabrirEstados = contadorabrirEstados + 1
                        estado = 11
                    ElseIf examinar_ascii_1 = 93 Then ' ] 
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        contadorcerrarEstados = contadorcerrarEstados + 1
                        estado = 11

                    Else
                        metodoErrores2(examinar_1, fila_1, columna_1)

                    End If

                Case 10
                    If (examinar_ascii_1 > 64 And examinar_ascii_1 < 91) Or (examinar_ascii_1 > 96 And examinar_ascii_1 < 123) Then 'letras minuscula'
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 10
                    ElseIf (examinar_ascii_1 > 47 And examinar_ascii_1 < 58) Then  'numeros'
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 10
                    ElseIf examinar_ascii_1 = 95 Then ' _
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 10
                    Else
                        metodoestadoInicial(caracter_1, fila_1, columna_1, caracterAUX_1, caracterAUX_2) 'llenar campos de envio lexico'
                        caracter_1 = ""
                        puntero_1 = puntero_1 - 1
                        estado = 9
                    End If

                Case 11
                    metodoErrores(caracter_1, fila_1, columna_1) 'llenar campos de envio lexico'
                    caracter_1 = ""
                    puntero_1 = puntero_1 - 1

                    If contadorcerrarEstados = 4 Then
                        Console.WriteLine("CIERRE DE ESTADO INICIAL")
                        estado = 9
                    Else
                        estado = 9
                    End If

            End Select


        Next
        Console.WriteLine(contadorcerrarEstados)
    End Sub

    Public Sub metodoErrores(ByVal examinar_1 As String, ByVal fila_1 As Integer, ByVal columna_1 As Integer)
        contador_error_1 = contador_error_1 + 1
        dgvErrores.Rows.Add(contador_error_1, examinar_1, columna_1, fila_1)
    End Sub

    Public Sub metodoErrores2(ByVal examinar_1 As String, ByVal fila_1 As Integer, ByVal columna_1 As Integer)
        contador_error_2 = contador_error_2 + 1
        dgvErrores2.Rows.Add(contador_error_2, examinar_1, columna_1, fila_1)
    End Sub


    'comprobar si hay numeros en un terminal
    Public Sub metodoSimbolosTerminales(ByVal caracter_1 As String, ByVal fila_1 As Integer, ByVal columna_1 As Integer, ByVal caracterAUX_1 As String, ByVal caracterAUX_2 As String)
        Dim var_caracter As String = ""
        Dim var_tipotoke As String = ""
        Dim var_notoken As Integer = 0
        Dim var_posicion As String = ""
        Dim examinar_ascii_1 As Integer = 0
        Dim examinar_1 As String = ""
        Dim estado As Integer = 0
        Dim haynumero As Boolean = False

        contador_token = contador_token + 1
        var_caracter = caracter_1
        var_tipotoke = "Identificador"
        var_notoken = 6
        var_posicion = "fila" & fila_1 & "columna" & columna_1

        Dim texto_1 As String = caracter_1.ToCharArray 'me sapara todos los caracter del area analizar para que vaya analizando caracter por caracter

        For puntero_1 As Integer = 0 To caracter_1.Length - 1 'ciclo para que vaya anilizando cada caracter y me de el codigo ascii
            examinar_1 = texto_1(puntero_1)
            examinar_ascii_1 = Asc(examinar_1)

            Select Case estado

                Case 0
                    If (examinar_ascii_1 > 47 And examinar_ascii_1 < 58) Then 'numeros'
                        haynumero = True
                    Else

                    End If
            End Select
        Next

        If haynumero = True Then
            contador_error_2 = contador_error_2 + 1
            dgvErrores2.Rows.Add(contador_error_2, caracter_1 & " -> TERMINAL contiene Numeros!", columna_1, fila_1)
        ElseIf caracter_1.Length > 1 Then
            contador_error_2 = contador_error_2 + 1
            dgvErrores2.Rows.Add(contador_error_2, caracter_1 & " -> TERMINAL es una cadena!", columna_1, fila_1)
        ElseIf haynumero = False Then
            dgvSimbolos.Rows.Add(contador_token, caracter_1, "TERMINAL", "uso")
        End If

    End Sub

    'para ver si hay error en los no terminales segun la expresion regular
    Public Sub metodoSimbolosNoTerminales(ByVal caracter_1 As String, ByVal fila_1 As Integer, ByVal columna_1 As Integer, ByVal caracterAUX_1 As String, ByVal caracterAUX_2 As String)
        Dim var_caracter As String = ""
        Dim var_tipotoke As String = ""
        Dim var_notoken As Integer = 0
        Dim var_posicion As String = ""

        'la expresion regular definida para los no terminales es: LETRA("_")?(LETRA|DIGITO)*

        If (caracter_1.Substring(0, 1) = "0") Or (caracter_1.Substring(0, 1) = "1") Or (caracter_1.Substring(0, 1) = "2") Or (caracter_1.Substring(0, 1) = "3") Or (caracter_1.Substring(0, 1) = "4") Or (caracter_1.Substring(0, 1) = "5") Or (caracter_1.Substring(0, 1) = "6") Or (caracter_1.Substring(0, 1) = "7") Or (caracter_1.Substring(0, 1) = "8") Or (caracter_1.Substring(0, 1) = "9") Then
            contador_error_2 = contador_error_2 + 1
            dgvErrores2.Rows.Add(contador_error_2, "ERROR SINTACTICO: " & caracter_1 & " -> NO TERMINAL no cumple con la expresion regular.", columna_1, fila_1)
        ElseIf (caracter_1.Substring(0, 1) = "_") Then
            contador_error_2 = contador_error_2 + 1
            dgvErrores2.Rows.Add(contador_error_2, "ERROR SINTACTICO: " & caracter_1 & " -> NO TERMINAL no cumple con la expresion regular.", columna_1, fila_1)
        ElseIf caracter_1.IndexOf("_", 0) > 1 Then
            contador_error_2 = contador_error_2 + 1
            dgvErrores2.Rows.Add(contador_error_2, "ERROR SINTACTICO: " & caracter_1 & " -> NO TERMINAL no cumple con la expresion regular.", columna_1, fila_1)
        ElseIf caracter_1.IndexOf("_", 0) = 1 And caracter_1.Length = 2 Then
            contador_error_2 = contador_error_2 + 1
            dgvErrores2.Rows.Add(contador_error_2, "ERROR SINTACTICO: " & caracter_1 & " -> NO TERMINAL no cumple con la expresion regular.", columna_1, fila_1)
        Else
            contador_token = contador_token + 1
            var_caracter = caracter_1
            var_tipotoke = "Identificador"
            var_notoken = 6
            var_posicion = "fila" & fila_1 & "columna" & columna_1

            dgvSimbolos.Rows.Add(contador_token, caracter_1, "NO TERMINAL", "uso")
        End If


        

    End Sub

    'me agrega las transiciones a la tabla dgvTrans
    Public Sub metodoTransiciones(ByVal caracter_1 As String, ByVal fila_1 As Integer, ByVal columna_1 As Integer, ByVal caracterAUX_1 As String, ByVal caracterAUX_2 As String)

        Dim var_caracter As String = ""
        Dim var_tipotoke As String = ""
        Dim var_notoken As Integer = 0
        Dim var_posicion As String = ""


        contador_token = contador_token + 1
        var_caracter = caracter_1
        var_tipotoke = "Identificador"
        var_notoken = 6
        var_posicion = "fila" & fila_1 & "columna" & columna_1

        'dgvSimbolos.Rows.Add(contador_token, caracter_1, columna_1, fila_1)
        dgvTrans.Rows.Add(caracter_1)

    End Sub

    'paras saber cual es el estado inicial
    Public Sub metodoestadoInicial(ByVal caracter_1 As String, ByVal fila_1 As Integer, ByVal columna_1 As Integer, ByVal caracterAUX_1 As String, ByVal caracterAUX_2 As String)

        Dim var_caracter As String = ""
        Dim var_tipotoke As String = ""
        Dim var_notoken As Integer = 0
        Dim var_posicion As String = ""


        contador_token = contador_token + 1
        var_caracter = caracter_1
        var_tipotoke = "Identificador"
        var_notoken = 6
        var_posicion = "fila" & fila_1 & "columna" & columna_1

        dgvestadoInicial.Rows.Add(caracter_1)
        existeInicial(caracter_1, fila_1, columna_1) 'para verificar si existe el estado inicial en los no terminales
    End Sub

    'PARA VERIFICAR SI EL ESTADO INICIAL EXISTE EN LA TABLA DE SIMBOLOS
    Public Sub existeInicial(ByVal cadena As String, ByVal fila As Integer, ByVal columna As Integer)

        'para verificar si existe el estado inicial
        Dim existe As Boolean = False


        For Each row2 As DataGridViewRow In dgvSimbolos.Rows 'me recorre la tabla de simbolos
            If row2.Cells(1).Value = cadena And row2.Cells(2).Value = "NO TERMINAL" Then
                existe = True
            End If

        Next

        'si el estado inicial no existe en los no terminales entonces me lo agrega como error
        If existe = False Then
            contador_error_2 = contador_error_2 + 1
            dgvErrores2.Rows.Add(contador_error_2, cadena & " -> Estado inicial no existe en los NO TERMINAL", columna, fila)
        End If

    End Sub

    'para saber en que transicion de usan los simbolos terminales 
    Public Sub terminalesTrancisiones()

        Dim contador As Integer = 0
        Dim cadenas As String = ""

        For Each row As DataGridViewRow In dgvSimbolos.Rows 'me recorre la tabla se simbolos
            contador = contador + 1
            Try 'try por si hay excepciones
                If row.Cells(2).Value.ToString = "TERMINAL" Then

                    Console.WriteLine(row.Cells(1).Value & "es terminal")

                    For Each row2 As DataGridViewRow In dgvTrans.Rows 'me recorre la tabla de transiciones
                        Try 'try por si hay excepciones
                            If row2.Cells(0).Value.ToString.Contains(row.Cells(1).Value) Then 'si la cadena de una celda de la tabla transiciones contiene la cadena de la tabla simbolos me lo imprime
                                Console.WriteLine(row.Cells(1).Value & " " & row2.Cells(0).Value)
                                cadenas = cadenas & "{" & row2.Cells(0).Value & "} ,"

                            End If

                        Catch ex As NullReferenceException

                        End Try
                    Next
                    dgvSimbolos.Rows(contador - 1).Cells(3).Value = cadenas
                    cadenas = ""
                End If

            Catch ex As NullReferenceException

            End Try


        Next
    End Sub

    'para saber en que transicion de usan los simbolos no terminales 
    Public Sub noTerminalesTrancisiones()

        Dim contador As Integer = 0
        Dim cadenas As String = ""

        For Each row As DataGridViewRow In dgvSimbolos.Rows 'me recorre la tabla se simbolos
            contador = contador + 1
            Try 'try por si hay excepciones
                If row.Cells(2).Value.ToString = "NO TERMINAL" Then

                    Console.WriteLine(row.Cells(1).Value & "no terminal")

                    For Each row2 As DataGridViewRow In dgvTrans.Rows 'me recorre la tabla de transiciones
                        Try 'try por si hay excepciones
                            If row2.Cells(0).Value.ToString.Contains(row.Cells(1).Value) Then 'si la cadena de una celda de la tabla transiciones contiene la cadena de la tabla simbolos me lo imprime
                                Console.WriteLine(row.Cells(1).Value & " " & row2.Cells(0).Value)
                                cadenas = cadenas & "{" & row2.Cells(0).Value & "} ,"

                            End If

                        Catch ex As NullReferenceException

                        End Try
                    Next
                    dgvSimbolos.Rows(contador - 1).Cells(3).Value = cadenas
                    cadenas = ""
                End If

            Catch ex As NullReferenceException

            End Try


        Next
    End Sub

    'corchetes que abre
    Public Function contadorcorchetes() As Integer

        corchete_que_abre = 0

        Dim examinar_ascii_1 As Integer = 0
        Dim estado As Integer = 0
        Dim examinar_1 As String = ""
        Dim texto_1 As String = areaAnalizar.Text.ToCharArray 'me sapara todos los caracter del area analizar para que vaya analizando caracter por caracter

        For puntero_1 As Integer = 0 To areaAnalizar.TextLength - 1 'ciclo para que vaya anilizando cada caracter y me de el codigo ascii
            examinar_1 = texto_1(puntero_1)
            examinar_ascii_1 = Asc(examinar_1)
            'Console.WriteLine(examinar_ascii_1)

            Select estado

                Case 0
                    If examinar_ascii_1 = 91 Then  'salto de linea'
                        corchete_que_abre = corchete_que_abre + 1
                    End If

            End Select
        Next

        Return corchete_que_abre

    End Function

    'corchetes que cierran
    Public Function contadorcorchetes2() As Integer

        corchete_que_cierra = 0

        Dim examinar_ascii_1 As Integer = 0
        Dim estado As Integer = 0
        Dim examinar_1 As String = ""
        Dim texto_1 As String = areaAnalizar.Text.ToCharArray 'me sapara todos los caracter del area analizar para que vaya analizando caracter por caracter

        For puntero_1 As Integer = 0 To areaAnalizar.TextLength - 1 'ciclo para que vaya anilizando cada caracter y me de el codigo ascii
            examinar_1 = texto_1(puntero_1)
            examinar_ascii_1 = Asc(examinar_1)
            'Console.WriteLine(examinar_ascii_1)

            Select Case estado

                Case 0
                    If examinar_ascii_1 = 93 Then  'salto de linea'
                        corchete_que_cierra = corchete_que_cierra + 1
                    End If

            End Select
        Next

        Return corchete_que_cierra

    End Function

    'para saber cuantas comas tienen las transiciones
    Public Sub cadenaGrafo()

        Dim examinar_ascii_1 As Integer = 0
        Dim examinar_1 As String = ""
        Dim estado As Integer = 0
        Dim contador As Integer = 0
        Dim cadenadoscomas As String = ""
        Dim cadenaunacoma As String = ""

        For Each row As DataGridViewRow In dgvTrans.Rows ' recorro la tabla de transiciones

            Try
                Dim texto_1 As String = row.Cells(0).Value.ToString.ToCharArray 'me sapara todos los caracter del area analizar para que vaya analizando caracter por caracter (de cada celda)

                For puntero_1 As Integer = 0 To row.Cells(0).Value.ToString.Length - 1 'ciclo para que vaya anilizando cada caracter y me de el codigo ascii
                    examinar_1 = texto_1(puntero_1)
                    examinar_ascii_1 = Asc(examinar_1)

                    Select Case estado

                        Case 0
                            If (examinar_ascii_1 = 44) Then ' ,   si la cadena tiene comas, me aumenta el contador
                                contador = contador + 1
                            Else

                            End If
                    End Select

                Next

                If contador = 2 Then
                    Console.WriteLine(contador & " contiene dos comas")
                    Console.WriteLine(row.Cells(0).Value)
                    cadenadoscomas = cadenadoscomas + row.Cells(0).Value + ","
                ElseIf contador = 1 Then
                    Console.WriteLine(contador & " contiene una coma")
                    Console.WriteLine(row.Cells(0).Value)
                    cadenaunacoma = cadenaunacoma + row.Cells(0).Value + ","
                ElseIf contador >= 3 Then
                    Console.WriteLine(contador & " contiene mas comas")
                    Console.WriteLine(row.Cells(0).Value)
                    contador_error_2 = contador_error_2 + 1
                    dgvErrores2.Rows.Add(contador_error_2, "ERROR SINTACTICO: " & "{" & row.Cells(0).Value & "}" & " -> TRANSICION contiene mas de tres terminales.", "---", "---")
                ElseIf contador = 0 Then
                    contador_error_2 = contador_error_2 + 1
                    dgvErrores2.Rows.Add(contador_error_2, "ERROR SINTACTICO: " & "{" & row.Cells(0).Value & "}" & " -> TRANSICION no contiene relacion de terminales.", "---", "---")
                End If
                contador = 0
            Catch ex As NullReferenceException

            End Try

        Next

        cadenaGrafo2(cadenadoscomas)
        cadenaGrafo3(cadenaunacoma)

    End Sub

    'PARA CUANDO HAY DOS COMAS EN UNA TRANSICION
    Public Sub cadenaGrafo2(ByVal entrada As String)

        Dim caracterAUX_1 As String = ""
        Dim caracterAUX_2 As String = ""
        Dim caracter_1 As String = ""
        Dim columna_1 As Integer = 1
        Dim fila_1 As Integer = 1
        Dim examinar_ascii_1 As Integer = 0
        Dim estado As Integer = 0
        Dim examinar_1 As String = ""

        Dim texto_1 As String = entrada.ToCharArray 'me sapara todos los caracter del area analizar para que vaya analizando caracter por caracter

        For puntero_1 As Integer = 0 To entrada.Length - 1 'ciclo para que vaya anilizando cada caracter y me de el codigo ascii
            examinar_1 = texto_1(puntero_1)
            examinar_ascii_1 = Asc(examinar_1)

            Select Case estado

                Case 0

                    If examinar_ascii_1 = 10 Then  'salto de linea'
                        fila_1 = fila_1 + 1
                        columna_1 = 1
                        estado = 0
                    ElseIf examinar_ascii_1 = 32 Then   'espacio en blanco'
                        columna_1 = columna_1 + 1
                        estado = 0
                    ElseIf examinar_ascii_1 = 9 Then 'tabulacion'
                        columna_1 = columna_1 + 1
                        estado = 0
                    ElseIf (examinar_ascii_1 > 32 And examinar_ascii_1 < 44) Or (examinar_ascii_1 > 44 And examinar_ascii_1 < 91) Or (examinar_ascii_1 > 93 And examinar_ascii_1 < 255) Or (examinar_ascii_1 = 92) Then 'letras'
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 1
                    ElseIf examinar_ascii_1 = 44 Then ' ,
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 2
                    Else


                    End If

                Case 1
                    If (examinar_ascii_1 > 32 And examinar_ascii_1 < 44) Or (examinar_ascii_1 > 44 And examinar_ascii_1 < 91) Or (examinar_ascii_1 > 93 And examinar_ascii_1 < 255) Or (examinar_ascii_1 = 92) Then 'letras'
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 1
                    Else
                        dgvtransdoscomas.Rows.Add(caracter_1)
                        caracter_1 = ""
                        puntero_1 = puntero_1 - 1
                        estado = 0
                    End If

                Case 2
                    caracter_1 = ""
                    puntero_1 = puntero_1 - 1
                    estado = 0

            End Select
        Next

    End Sub

    'PARA CUANDO HAY UNA COMA EN UNA TRANSICION
    Public Sub cadenaGrafo3(ByVal entrada As String)

        Dim caracterAUX_1 As String = ""
        Dim caracterAUX_2 As String = ""
        Dim caracter_1 As String = ""
        Dim columna_1 As Integer = 1
        Dim fila_1 As Integer = 1
        Dim examinar_ascii_1 As Integer = 0
        Dim estado As Integer = 0
        Dim examinar_1 As String = ""

        Dim texto_1 As String = entrada.ToCharArray 'me sapara todos los caracter del area analizar para que vaya analizando caracter por caracter

        For puntero_1 As Integer = 0 To entrada.Length - 1 'ciclo para que vaya anilizando cada caracter y me de el codigo ascii
            examinar_1 = texto_1(puntero_1)
            examinar_ascii_1 = Asc(examinar_1)

            Select Case estado

                Case 0

                    If examinar_ascii_1 = 10 Then  'salto de linea'
                        fila_1 = fila_1 + 1
                        columna_1 = 1
                        estado = 0
                    ElseIf examinar_ascii_1 = 32 Then   'espacio en blanco'
                        columna_1 = columna_1 + 1
                        estado = 0
                    ElseIf examinar_ascii_1 = 9 Then 'tabulacion'
                        columna_1 = columna_1 + 1
                        estado = 0
                    ElseIf (examinar_ascii_1 > 32 And examinar_ascii_1 < 44) Or (examinar_ascii_1 > 44 And examinar_ascii_1 < 91) Or (examinar_ascii_1 > 93 And examinar_ascii_1 < 255) Or (examinar_ascii_1 = 92) Then 'letras'
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 1
                    ElseIf examinar_ascii_1 = 44 Then ' ,
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 2
                    Else


                    End If

                Case 1
                    If (examinar_ascii_1 > 32 And examinar_ascii_1 < 44) Or (examinar_ascii_1 > 44 And examinar_ascii_1 < 91) Or (examinar_ascii_1 > 93 And examinar_ascii_1 < 255) Or (examinar_ascii_1 = 92) Then 'letras'
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 1
                    Else
                        dgvtransunacoma.Rows.Add(caracter_1)
                        caracter_1 = ""
                        puntero_1 = puntero_1 - 1
                        estado = 0
                    End If

                Case 2
                    caracter_1 = ""
                    puntero_1 = puntero_1 - 1
                    estado = 0

            End Select
        Next

    End Sub

    'PARA VERICAR SI LOS SIMBOLOS(TERMINAL O NO TERMINAL) DE LAS TRANSICIONES, EXISTEN EN LA TABLA DE SIMBOLOS (CUANDO HAY DOS COMAS)
    Public Sub cadenaGrafo4()

        Dim contador As Integer = 0 'contador para saber en que fila estoy
        Dim estados As String = ""
        'para verificar si existen los terminales antes de graficar
        Dim noterminal1 As Boolean = False
        Dim noterminal2 As Boolean = False
        Dim terminal As Boolean = False


        For Each row As DataGridViewRow In dgvtransdoscomas.Rows 'me recorre la tabla de transiciones que tienen dos comas
            contador = contador + 1
            If contador Mod 3 = 0 Then
                
                For Each row2 As DataGridViewRow In dgvSimbolos.Rows 'me recorre la tabla de simbolos
                    Console.WriteLine(row2.Cells(1).Value)
                    'si el primer noterminal esta en la tabla simbolos
                    If (row2.Cells(1).Value = dgvtransdoscomas.Rows(contador - 3).Cells(0).Value) And (row2.Cells(2).Value = "NO TERMINAL") Then
                        noterminal1 = True
                    End If
                    
                Next

                For Each row3 As DataGridViewRow In dgvSimbolos.Rows 'me recorre la tabla de simbolos
                    'si el segundo noterminal esta en la tabla simbolos
                    If (row3.Cells(1).Value = dgvtransdoscomas.Rows(contador - 1).Cells(0).Value) And (row3.Cells(2).Value = "NO TERMINAL") Then
                        noterminal2 = True
                    End If
                   
                Next

                For Each row4 As DataGridViewRow In dgvSimbolos.Rows 'me recorre la tabla de simbolos
                    'si el terminal esta en la tabla simbolos
                    If (row4.Cells(1).Value = dgvtransdoscomas.Rows(contador - 2).Cells(0).Value) And (row4.Cells(2).Value = "TERMINAL") Then
                        terminal = True
                    End If
                   
                Next

                'para verificar los tres simbolos estan declarados antes de graficarlos
                If (noterminal1 = True) And (noterminal2 = True) And (terminal = True) Then
                    estados = dgvtransdoscomas.Rows(contador - 3).Cells(0).Value + "->" + dgvtransdoscomas.Rows(contador - 1).Cells(0).Value + " [ label = " + Chr(34) + dgvtransdoscomas.Rows(contador - 2).Cells(0).Value + Chr(34) + " ];"
                    dgvestadosgrafo.Rows.Add(estados)
                    estados = ""
                    noterminal1 = False
                    noterminal2 = False
                    terminal = False
                Else
                    contador_error_2 = contador_error_2 + 1
                    dgvErrores2.Rows.Add(contador_error_2, "{" & dgvtransdoscomas.Rows(contador - 3).Cells(0).Value & "," & dgvtransdoscomas.Rows(contador - 2).Cells(0).Value & "," & dgvtransdoscomas.Rows(contador - 1).Cells(0).Value & "}" & " -> Algun TERMINAL o NO TERMINAL no esta declarado", "-----", "-----")
                    noterminal1 = False
                    noterminal2 = False
                    terminal = False
                End If

            End If


        Next
    End Sub

    'PARA CUANDO HAY UNA COMA (ESTADO EPSILON)
    Public Sub cadenaGrafo5()

        Dim contador As Integer = 0 'contador para saber en que fila estoy
        Dim estados As String = ""
        Dim noterminal1 As Boolean = False
        Dim noterminal2 As Boolean = False


        For Each row As DataGridViewRow In dgvtransunacoma.Rows
            contador = contador + 1
            If contador Mod 2 = 0 Then

                For Each row2 As DataGridViewRow In dgvSimbolos.Rows 'me recorre la tabla de simbolos
                    'si el noterminal esta en la tabla simbolos
                    If (row2.Cells(1).Value = dgvtransunacoma.Rows(contador - 2).Cells(0).Value) And (row2.Cells(2).Value = "NO TERMINAL") Then
                        noterminal1 = True
                    End If

                Next

                For Each row3 As DataGridViewRow In dgvSimbolos.Rows 'me recorre la tabla de simbolos
                    'si el no terminal esta en la tabla simbolos
                    If (row3.Cells(1).Value = dgvtransunacoma.Rows(contador - 1).Cells(0).Value) And (row3.Cells(2).Value = "NO TERMINAL") Then
                        noterminal2 = True
                    End If

                Next

                If (noterminal1 = True) And (noterminal2 = True) Then
                    estados = dgvtransunacoma.Rows(contador - 2).Cells(0).Value + " -> " + dgvtransunacoma.Rows(contador - 1).Cells(0).Value + " [ label = " + Chr(34) + "Epsilon" + Chr(34) + " ];"
                    dgvestadoepsilon.Rows.Add(estados)
                    estados = ""
                    noterminal1 = False
                    noterminal2 = False
                Else
                    contador_error_2 = contador_error_2 + 1
                    dgvErrores2.Rows.Add(contador_error_2, "{" & dgvtransunacoma.Rows(contador - 2).Cells(0).Value & "," & dgvtransunacoma.Rows(contador - 1).Cells(0).Value & "}" & " -> Algun NO TERMINAL no esta declarado", "-----", "-----")
                    noterminal1 = False
                    noterminal2 = False
                End If

                
            End If
        Next
    End Sub

    'metodo para graficar
    Public Sub graficar()
        Dim sw As New System.IO.StreamWriter("grafo.txt")
        sw.WriteLine(generarGrafo())
        sw.Close()
        Dim prog As VariantType
        prog = Shell("C:\Program Files (x86)\Graphviz2.38\bin\dot.exe -Tjpg grafo.txt -o grafo.jpg", 1)

    End Sub

    'ME LEE LAS CADENAS Y LAS CONCATENA PARA GENERAR EL GRAFO
    Public Function generarGrafo() As String
        Dim generar As String = ""

        generar += "digraph grafica{" & vbCrLf
        generar += "rankdir=LR;"
        generar += "size=" & Chr(34) & "8,5" & Chr(34)

        generar += "node [shape = doublecircle, color = blue]; " + dgvestadoInicial.Rows(0).Cells(0).Value + ";"

        generar += "node [shape = circle];"

        For Each row As DataGridViewRow In dgvestadoepsilon.Rows

            generar = generar + row.Cells(0).Value

        Next

        generar += vbCrLf

        For Each row2 As DataGridViewRow In dgvestadosgrafo.Rows

            generar = generar + row2.Cells(0).Value

        Next

        generar += "}"

        Console.WriteLine(generar)
        Return generar
    End Function

    'ME ABRE EL GRAFO YA GENERADO
    Public Sub abrirGrafo()
        Dim loPSI As New ProcessStartInfo
        Dim proceso As New Process
        loPSI.FileName = "grafo.jpg"

        Try
            proceso = Process.Start(loPSI)

        Catch Exp As Exception
            MessageBox.Show(Exp.Message, "XXXX", MessageBoxButtons.OK, MessageBoxIcon.Information)
        End Try
    End Sub

    'PARA LIMPIAR TODAS LAS TABLAS
    Public Sub metodoLimpiarTodo()
        dgvSimbolos.Rows.Clear()
        dgvErrores.Rows.Clear()
        dgvErrores2.Rows.Clear()
        dgvTrans.Rows.Clear()
        dgvestadoInicial.Rows.Clear()
        dgvtransdoscomas.Rows.Clear()
        dgvtransunacoma.Rows.Clear()
        dgvestadosgrafo.Rows.Clear()
        dgvestadoepsilon.Rows.Clear()
    End Sub

    'METODO QUE INICIA EL ANALISIS
    Public Sub inicioAnalizar()

        'para que me reinicie los contadores
        contador_token = 0
        contador_error_1 = 0
        contador_error_2 = 0

        'para que me limpie las tablas antes de iniciar analisis
        metodoLimpiarTodo()

        metodo_analizar()
        terminalesTrancisiones()
        noTerminalesTrancisiones()
        cadenaGrafo()
        cadenaGrafo4()
        cadenaGrafo5()

        'por si hay errores lexicos me tira error
        If dgvErrores2.Rows.Count >= 2 Then
            MessageBox.Show("No se puede generar GRAFO, Por errorres LEXICOS o errores SINTACTICOS.", "ERROR", MessageBoxButtons.OK, MessageBoxIcon.Error)

        Else
            graficar()
            abrirGrafo()
        End If

        MetodoHTML_simbolos()
        MetodoHTML_Errores_Lexemas()

    End Sub

    'PARA REPORTE HTML SIMBOLOS
    Public Sub MetodoHTML_simbolos()

        Dim ii As Integer = 0
        Dim DireccionDocumento As String = My.Computer.FileSystem.SpecialDirectories.Desktop
        My.Computer.FileSystem.CreateDirectory(DireccionDocumento & "\Reportes")
        Dim Path As String = DireccionDocumento & "\Reportes\Reporte_Simbolos.html"
        Dim Archivo As FileInfo = New FileInfo(Path)
        Dim Escribir As StreamWriter
        Escribir = File.CreateText(Path)
        Escribir.WriteLine("<html>")
        Escribir.WriteLine("<head><title>Reporte De Simbolos</title></head>")
        Escribir.WriteLine("<body bgcolor=white text=Black background=FondoEj3.gif >")
        Escribir.WriteLine("<h1><center>Reporte Simbolos</center></h1>")
        Escribir.WriteLine("<br>")
        Escribir.WriteLine("<center>")
        Escribir.WriteLine("<table border= 1 width= 500 bgcolor= SILVER>")
        Escribir.WriteLine("<tr>")
        Escribir.WriteLine("<th>No.</th>")
        Escribir.WriteLine("<th>Simbolo</th>")
        Escribir.WriteLine("<th>Terminal/No Terminal</th>")
        Escribir.WriteLine("<th>Uso</th>")

        Escribir.WriteLine("</tr>")
        Dim PosicionError As Integer = 0
        For Each row As DataGridViewRow In dgvSimbolos.Rows

            Escribir.WriteLine("<tr>")
            Escribir.WriteLine("<th>" & row.Cells(0).Value & "</th>")
            Escribir.WriteLine("<th>" & row.Cells(1).Value & "</th>")
            Escribir.WriteLine("<th>" & row.Cells(2).Value & "</th>")
            Escribir.WriteLine("<th>" & row.Cells(3).Value & "</th>")

            Escribir.WriteLine("</tr>")

        Next
        Escribir.WriteLine("</table><br>")
        Escribir.WriteLine("</center></body></html>")
        Escribir.Flush()
        Escribir.Close()

        Dim proceso As New System.Diagnostics.Process
        With proceso
            .StartInfo.FileName = Path
            .Start()
        End With


    End Sub

    'PARA REPORTE DE ERRORES Y DE TOKENS
    Public Sub MetodoHTML_Errores_Lexemas()

        Dim ii As Integer = 0
        Dim DireccionDocumento As String = My.Computer.FileSystem.SpecialDirectories.Desktop
        My.Computer.FileSystem.CreateDirectory(DireccionDocumento & "\Reportes")
        Dim Path As String = DireccionDocumento & "\Reportes\Reporte_Errores.html"
        Dim Archivo As FileInfo = New FileInfo(Path)
        Dim Escribir As StreamWriter
        Escribir = File.CreateText(Path)

        Escribir.WriteLine("<html>")
        Escribir.WriteLine("<head><title>Reporte De Errores</title></head>")
        Escribir.WriteLine("<body bgcolor=white text=Black background=FondoEj3.gif >")

        'para tabla errores
        Escribir.WriteLine("<h1><center>Reporte Errores</center></h1>")
        Escribir.WriteLine("<br>")
        Escribir.WriteLine("<center>")
        Escribir.WriteLine("<table border= 1 width= 500 bgcolor= SILVER>")
        Escribir.WriteLine("<tr>")
        Escribir.WriteLine("<th>No.</th>")
        Escribir.WriteLine("<th>Error</th>")
        Escribir.WriteLine("<th>Columna</th>")
        Escribir.WriteLine("<th>Linea</th>")

        Escribir.WriteLine("</tr>")

        For Each row As DataGridViewRow In dgvErrores2.Rows

            Escribir.WriteLine("<tr>")
            Escribir.WriteLine("<th>" & row.Cells(0).Value & "</th>")
            Escribir.WriteLine("<th>" & row.Cells(1).Value & "</th>")
            Escribir.WriteLine("<th>" & row.Cells(2).Value & "</th>")
            Escribir.WriteLine("<th>" & row.Cells(3).Value & "</th>")


            Escribir.WriteLine("</tr>")

        Next
        Escribir.WriteLine("</table><br>")

        Escribir.WriteLine("<p></p>")

        'para la tabla otros errores
        Escribir.WriteLine("<h1><center>Reporte de Tokens</center></h1>")
        Escribir.WriteLine("<br>")
        Escribir.WriteLine("<center>")
        Escribir.WriteLine("<table border= 1 width= 500 bgcolor= SILVER>")
        Escribir.WriteLine("<tr>")
        Escribir.WriteLine("<th>No.</th>")
        Escribir.WriteLine("<th>Token</th>")
        Escribir.WriteLine("<th>Columna</th>")
        Escribir.WriteLine("<th>Fila</th>")

        Escribir.WriteLine("</tr>")

        For Each row As DataGridViewRow In dgvErrores.Rows

            Escribir.WriteLine("<tr>")
            Escribir.WriteLine("<th>" & row.Cells(0).Value & "</th>")
            Escribir.WriteLine("<th>" & row.Cells(1).Value & "</th>")
            Escribir.WriteLine("<th>" & row.Cells(2).Value & "</th>")
            Escribir.WriteLine("<th>" & row.Cells(3).Value & "</th>")

            Escribir.WriteLine("</tr>")

        Next
        Escribir.WriteLine("</table><br>")

        Escribir.WriteLine("</center></body></html>")
        Escribir.Flush()
        Escribir.Close()

        Dim proceso As New System.Diagnostics.Process
        With proceso
            .StartInfo.FileName = Path
            .Start()
        End With

    End Sub

    Public Sub metodo_abrir()
        Dim OpenFileDialog1 As New OpenFileDialog()
        Dim Alltext As String = "", LineOfText As String = ""
        OpenFileDialog1.Filter = "Text files (*.lfp)|*.lfp"
        OpenFileDialog1.ShowDialog()
        If OpenFileDialog1.FileName <> "" Then
            Try
                FileOpen(1, OpenFileDialog1.FileName, OpenMode.Input)
                Do Until EOF(1)
                    LineOfText = LineInput(1)
                    Alltext = Alltext & LineOfText & vbCrLf
                Loop
                'ArchivoToolStripMenuItem.Text = OpenFileDialog1.FileName'
                areaAnalizar.Text = Alltext
                areaAnalizar.Enabled = True
                'ToolStripMenuItem.Close.Enabled = True'

            Catch ex As Exception
                MsgBox("Error")
            Finally
                FileClose(1)
            End Try
        End If
    End Sub

    Public Sub metodo_guardar()
        Dim Save As New SaveFileDialog()
        Dim myStreamWriter As System.IO.StreamWriter
        Save.Filter = "Documento LFP [*.LFP*]|*.LFP*|Todos los archivos [*,*]|*,*"
        Save.CheckPathExists = True
        Save.Title = "Guardar"
        Save.ShowDialog(Me)
        Try
            myStreamWriter = System.IO.File.AppendText(Save.FileName & ".lfp")
            myStreamWriter.Write(areaAnalizar.Text)
            myStreamWriter.Flush()
        Catch ex As Exception

        End Try
    End Sub

    Public Sub metodo_guardarcomo()
        Dim Save As New SaveFileDialog()
        Dim myStreamWriter As System.IO.StreamWriter
        Save.Filter = "Documento LFP [*.lfprec*]|*.lfprec*|Todos los archivos [*,*]|*,*"
        Save.CheckPathExists = True
        Save.Title = "Guardar"
        Save.ShowDialog(Me)
        Try
            myStreamWriter = System.IO.File.AppendText(Save.FileName & ".lfp")
            myStreamWriter.Write(areaAnalizar.Text)
            myStreamWriter.Flush()
        Catch ex As Exception

        End Try
    End Sub

    Public Sub metodo_salir()
        Close()
    End Sub

    Private Sub bttlimpiar_Click(sender As Object, e As EventArgs) Handles bttlimpiar.Click
        metodoLimpiarTodo()
        areaAnalizar.Text = ""
    End Sub

    Private Sub AbrirToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles AbrirToolStripMenuItem.Click
        metodo_abrir()
    End Sub

    Private Sub GuardarToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles GuardarToolStripMenuItem.Click
        metodo_guardar()
    End Sub

    Private Sub GuardarComoToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles GuardarComoToolStripMenuItem.Click
        metodo_guardarcomo()
    End Sub

    Private Sub SalirToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SalirToolStripMenuItem.Click
        metodo_salir()
    End Sub

    Private Sub AnalizarLexicoToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles AnalizarLexicoToolStripMenuItem.Click
        corchete_que_abre = 0
        corchete_que_cierra = 0

        metodoLimpiarTodo()

        If contadorcorchetes() = 4 And contadorcorchetes2() = 4 Then
            inicioAnalizar()
        Else
            contador_error_2 = contador_error_2 + 1
            dgvErrores2.Rows.Add(contador_error_2, "ERROR SINTACTICO: -> Corchetes ']' '[' ", "--", "--")

        End If
    End Sub

    Private Sub ReporteDeSimbolosToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ReporteDeSimbolosToolStripMenuItem.Click

        Dim loPSI As New ProcessStartInfo
        Dim proceso As New Process
        loPSI.FileName = "C:\Users\GUDIEL\Desktop\Reportes\Reporte_Simbolos.html"

        Try
            proceso = Process.Start(loPSI)

        Catch Exp As Exception
            MessageBox.Show(Exp.Message, "XXXX", MessageBoxButtons.OK, MessageBoxIcon.Information)
        End Try

    End Sub

    Private Sub ReporteDeErrorToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ReporteDeErrorToolStripMenuItem.Click

        Dim loPSI As New ProcessStartInfo
        Dim proceso As New Process
        loPSI.FileName = "C:\Users\GUDIEL\Desktop\Reportes\Reporte_Errores.html"

        Try
            proceso = Process.Start(loPSI)

        Catch Exp As Exception
            MessageBox.Show(Exp.Message, "XXXX", MessageBoxButtons.OK, MessageBoxIcon.Information)
        End Try

    End Sub

    Private Sub GrafoToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles GrafoToolStripMenuItem.Click

        Dim loPSI As New ProcessStartInfo
        Dim proceso As New Process
        loPSI.FileName = "grafo.jpg"

        Try
            proceso = Process.Start(loPSI)

        Catch Exp As Exception
            MessageBox.Show(Exp.Message, "XXXX", MessageBoxButtons.OK, MessageBoxIcon.Information)
        End Try

    End Sub

    Private Sub ManualesToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ManualesToolStripMenuItem.Click

        Dim loPSI As New ProcessStartInfo
        Dim loPSI2 As New ProcessStartInfo
        Dim proceso As New Process
        Dim proceso2 As New Process

        loPSI.FileName = "manual_usuario.pdf"
        loPSI2.FileName = "manual_tecnico.pdf"

        Try
            proceso = Process.Start(loPSI)
            proceso2 = Process.Start(loPSI2)

        Catch Exp As Exception
            MessageBox.Show(Exp.Message, "XXXX", MessageBoxButtons.OK, MessageBoxIcon.Information)
        End Try

    End Sub

    Private Sub AcercaDeToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles AcercaDeToolStripMenuItem.Click

        Dim info As String = "Que es Analilisis lexico?" + vbLf + "El analizador léxico es la primera fase de un compilador, su principal función consiste en leer los caracteres de entrada y elaborar como salida una secuencia de componentes léxicos que utiliza el analizador sintáctico para hacer el análisis."
        Dim info2 As String = "Este software es para que pueda generar un autómata finito capaz de reconocer una gramática regular por su definición formal de un cuádruplo de elementos."
        Dim info3 As String = "Version de app: 2.0" + vbLf + "Desarrollado en Visual Basic, utilizando Visual Estudio 2013"
        MessageBox.Show("Realizado por: Christopher Alexander Acajabon Gudiel" + vbLf + vbLf + "Carnet: 201404278" + vbLf + vbLf + info3 + vbLf + vbLf + info + vbLf + vbLf + info2)

    End Sub
End Class
