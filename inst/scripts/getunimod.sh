wget http://www.unimod.org/xml/unimod.xml

new='./unimod.xml'
org='../extdata/unimod.xml'

if cmp -s "$new" "$org"
then
    echo "Unimod has not changed."
    rm $new
else
    echo "New unimod data. Replacing old"
    mv -b $new $org
fi


