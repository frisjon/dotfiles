copy customo in /usr/share/X11/xkb/symbols

add the layout to `/rules/evdev.xml`:
```
<layout>
  <configItem>
    <name>customo</name>
    <shortDescription>customolatam</shortDescription>
    <description>Customo Latam</description>
    <languageList>
    </languageList>
  </configItem>
  <variantList>
  </variantList>
</layout>
```

select the layout with `setxkbmap customo`
