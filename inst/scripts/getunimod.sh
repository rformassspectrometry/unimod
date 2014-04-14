wget http://www.unimod.org/xml/unimod_tables.xml

new='./unimod_tables.xml'
org='../extdata/unimod_tables.xml'

if cmp -s "$new" "$org"
then
    echo "Unimod has not changed."
    rm $new
else 
    echo "New unimod data. Replacing old"
    mv -b $new $org
fi


