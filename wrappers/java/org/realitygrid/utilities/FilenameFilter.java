/*
 * @(#)ExampleFileFilter.java	1.8 98/08/26
 *
 * Copyright 1998 by Sun Microsystems, Inc.,
 * 901 San Antonio Road, Palo Alto, California, 94303, U.S.A.
 * All rights reserved.
 *
 * This software is the confidential and proprietary information
 * of Sun Microsystems, Inc. ("Confidential Information").  You
 * shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement
 * you entered into with Sun.
 */

package org.realitygrid.utilities;

import java.io.File;
import java.util.Hashtable;
import java.util.Enumeration;
import javax.swing.*;
import javax.swing.filechooser.*;

/**
 * A convenience implementation of FileFilter that filters out
 * all files except for those type extensions that it knows about.
 *
 * Extensions are of the type ".foo", which is typically found on
 * Windows and Unix boxes, but not on Macintosh. Case is ignored.
 *
 * <p>Example - create a new filter that filers out all files
 * but gif and jpg image files:<p>
 *
 *     <code>
 *     JFileChooser chooser = new JFileChooser();<br>
 *     ExampleFileFilter filter = new ExampleFileFilter(new
 *                   String{"gif", "jpg"}, "GIF & JPEG Images");<br>
 *     chooser.addChoosableFileFilter(filter);<br>
 *     chooser.showOpenDialog(this);
 *     </code>
 *
 * @version 1.8 08/26/98
 * @author Jeff Dinkins - Sun Microsystems, Inc.
 * @author Robert Haines - University of Manchester, UK.
 */
public class FilenameFilter extends FileFilter {

    private static String TYPE_UNKNOWN = "Type Unknown";
    private static String HIDDEN_FILE = "Hidden File";

    private Hashtable filters = null;
    private String description = null;
    private String fullDescription = null;
    private boolean useExtensionsInDescription = true;

    /**
     * Creates a file filter. If no filters are added, then all
     * files are accepted.
     *
     * @see #addExtension(String)
     */
    public FilenameFilter() {
	this.filters = new Hashtable();
    }

    /**
     * Creates a file filter that accepts files with the given extension.<p>
     * Example: <code>new FilenameFilter("jpg");</code>
     *
     * @see #addExtension(String)
     */
    public FilenameFilter(String extension) {
	this(extension,null);
    }

    /**
     * Creates a file filter that accepts the given file type.<p>
     * Example: <code>new FilenameFilter("jpg", "JPEG Image Images");</code>
     *
     * <p>Note that the "." before the extension is not needed. If
     * provided, it will be ignored.
     *
     * @see #addExtension(String)
     */
    public FilenameFilter(String extension, String description) {
	this();
	if(extension!=null) addExtension(extension);
 	if(description!=null) setDescription(description);
    }

    /**
     * Creates a file filter from the given string array.<p>
     * Example: <code>new FilenameFilter(String {"gif", "jpg"});</code>
     *
     * <p>Note that the "." before the extension is not needed and
     * will be ignored.
     *
     * @see #addExtension(String)
     */
    public FilenameFilter(String[] filters) {
	this(filters, null);
    }

    /**
     * Creates a file filter from the given string array and description.
     * <p>Example:
     * <code>new FilenameFilter(String {"gif", "jpg"}, "Gif and JPG Images");</code>
     *
     * <p>Note that the "." before the extension is not needed and will be ignored.
     *
     * @see #addExtension(String)
     */
    public FilenameFilter(String[] filters, String description) {
	this();
	for (int i = 0; i < filters.length; i++) {
	    // add filters one by one
	    addExtension(filters[i]);
	}
 	if(description!=null) setDescription(description);
    }

    /**
     * Return true if this file should be shown in the directory pane,
     * false if it shouldn't.<p>
     *
     * Files that begin with "." are ignored.
     *
     * @see #getExtension(File)
     * @see FileFilter#accept
     */
    public boolean accept(File f) {
	if(f != null) {
	    if(f.isDirectory()) {
		return true;
	    }
	    String extension = getExtension(f);
	    if(extension != null && filters.get(getExtension(f)) != null) {
		return true;
	    };
	}
	return false;
    }

    /**
     * Return the extension portion of the file's name.
     *
     * @see #addExtension(String)
     * @see FileFilter#accept
     */
     public String getExtension(File f) {
	if(f != null) {
	    String filename = f.getName();
	    int i = filename.lastIndexOf('.');
	    if(i>0 && i<filename.length()-1) {
		return filename.substring(i+1).toLowerCase();
	    };
	}
	return null;
    }

    /**
     * Adds a filetype "dot" extension to filter against.
     *
     * For example: the following code will create a filter that filters
     * out all files except those that end in ".jpg" and ".tif":<p>
     *   <code>
     *   FilenameFilter filter = new FilenameFilter();
     *   filter.addExtension("jpg");
     *   filter.addExtension("tif");
     *   </code>
     *
     * <p>Note that the "." before the extension is not needed and will be ignored.
     */
    public void addExtension(String extension) {
	if(filters == null) {
	    filters = new Hashtable(5);
	}
	filters.put(extension.toLowerCase(), this);
	fullDescription = null;
    }


    /**
     * Returns the human readable description of this filter. For
     * example: "JPEG and GIF Image Files (*.jpg, *.gif)"
     *
     * @see #setDescription(String)
     * @see #setExtensionListInDescription(boolean)
     * @see #isExtensionListInDescription()
     * @see FileFilter#getDescription
     */
    public String getDescription() {
	if(fullDescription == null) {
	    if(description == null || isExtensionListInDescription()) {
 		fullDescription = description==null ? "(" : description + " (";
		// build the description from the extension list
		Enumeration extensions = filters.keys();
		if(extensions != null) {
		    fullDescription += "." + (String) extensions.nextElement();
		    while (extensions.hasMoreElements()) {
			fullDescription += ", " + (String) extensions.nextElement();
		    }
		}
		fullDescription += ")";
	    } else {
		fullDescription = description;
	    }
	}
	return fullDescription;
    }

    /**
     * Sets the human readable description of this filter. For
     * example: <code>filter.setDescription("Gif and JPG Images");</code>
     *
     * @see #getDescription()
     * @see #setExtensionListInDescription(boolean)
     * @see #isExtensionListInDescription()
     */
    public void setDescription(String description) {
	this.description = description;
	fullDescription = null;
    }

    /**
     * Determines whether the extension list (.jpg, .gif, etc) should
     * show up in the human readable description.
     *
     * Only relevent if a description was provided in the constructor
     * or using <code>setDescription();</code>
     *
     * @see #getDescription()
     * @see #setDescription(String)
     * @see #isExtensionListInDescription()
     */
    public void setExtensionListInDescription(boolean b) {
	useExtensionsInDescription = b;
	fullDescription = null;
    }

    /**
     * Returns whether the extension list (.jpg, .gif, etc) should
     * show up in the human readable description.
     *
     * Only relevent if a description was provided in the constructor
     * or using <code>setDescription();</code>
     *
     * @see #getDescription()
     * @see #setDescription(String)
     * @see #setExtensionListInDescription(boolean)
     */
    public boolean isExtensionListInDescription() {
	return useExtensionsInDescription;
    }
}
