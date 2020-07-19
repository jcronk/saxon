<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:counter="http://www.ivansinsurance.com/connect/counters"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    exclude-result-prefixes="#all"
    version="3.0">
    <xsl:use-package name="com.ivansinsurance.connect.counter"/>
    <xsl:template match="/">
        <xsl:variable name="counter" select="counter:incr(map {}, 'foo')"/>
        <Result>
            <xsl:sequence select="counter:read($counter, 'foo') "/>
        </Result>
    </xsl:template>
    
</xsl:stylesheet>