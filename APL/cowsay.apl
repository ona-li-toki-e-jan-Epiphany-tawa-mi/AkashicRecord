⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝
⍝ Cowsay in Dyalog APL.
⍝ 
⍝ Author: ona li toki e jan Epiphany tawa mi.

cow←'\' ' \' '   ^__^' '   (oo)\_______' '   (__)\       )\/\' '       ||----w |' '       ||     ||'
⍝ Takes text on the right and a number on the left. The text is split into strings sized to that 
⍝ number. If there are insufficent characters to make the final string, spaces will be added.
splitText←{⍵{↓⍵⍴⍺,' '\⍨(×/⍵)-⍴⍺}(⌈⍺÷⍨⍴⍵),⍺}
⍝ Takes an array of strings and places a text bubble border around them. The strings must be of the 
⍝ same length for it to be displayed correctly, which can be achieved with splitText.  
textBubbleize←{⍵{⍵{(⊂'/¯','¯\',⍨⍵/'¯'),(⊂'\_','_/',⍨⍵/'_'),⍨⍺}⍴⊃⍺}{'| ',⍵,' |'}¨⍵}
⍝ Cowsay. Takes text on the left and a number on the left. The number is the maximum width of the 
⍝ text in the text bubble. 
cowsay←{⍵{↑(⍵{⍵,⍨⍺/' '}¨cow),⍨textBubbleize ⍵ splitText ⍺}⍺⌊⍴⍵}

⍝ Oneliner version: {⍵{↑(⍵{⍵,⍨⍺/' '}¨'\' ' \' '   ^__^' '   (oo)\_______' '   (__)\       )\/\' '       ||----w |' '       ||     ||'),⍨{⍵{⍵{(⊂'/¯','¯\',⍨⍵/'¯'),(⊂'\_','_/',⍨⍵/'_'),⍨⍺}⍴⊃⍺}{'| ',⍵,' |'}¨⍵}⍵{⍵{↓⍵⍴⍺,' '\⍨(×/⍵)-⍴⍺}(⌈⍺÷⍨⍴⍵),⍺}⍺}⍺⌊⍴⍵}
