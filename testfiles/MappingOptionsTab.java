/*
 *  Gruntspud
 *
 *  Copyright (C) 2002 Brett Smith.
 *
 *  Written by: Brett Smith <t_magicthize@users.sourceforge.net>
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Library General Public License
 *  as published by the Free Software Foundation; either version 2 of
 *  the License, or (at your option) any later version.
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public
 *  License along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

package gruntspud.ui.preferences;

import gruntspud.Constants;
import gruntspud.GruntspudContext;
import gruntspud.ui.UIUtil;

import java.awt.BorderLayout;

import javax.swing.BorderFactory;

/**
 *  Description of the Class
 *
 *@author     magicthize
 *@created    26 May 2002
 */
public class MappingOptionsTab
    extends AbstractOptionsTab {
    private FileTypeMappingPane mappingPane;

    /**
     * Creates a new MappingOptionsTab object.
     */
    public MappingOptionsTab() {
        super("File Types", UIUtil.getCachedIcon(Constants.ICON_TOOL_FILE_TYPES));
    }

    /**
     * DOCUMENT ME!
     *
     * @param context DOCUMENT ME!
     */
    public void init(GruntspudContext context) {
        super.init(context);

        setTabLargeIcon(UIUtil.getCachedIcon(
            Constants.ICON_TOOL_LARGE_FILE_TYPES));
        setTabToolTipText("File type mappings");
        setTabMnemonic('c');
        setTabContext("Advanced");

        mappingPane = new FileTypeMappingPane(context);
        mappingPane.setBorder(BorderFactory.createEmptyBorder(2, 2, 6, 2));

        setLayout(new BorderLayout());
        add(mappingPane, BorderLayout.CENTER);
    }

    /**
     *  Description of the Method
     *
     *@return    Description of the Return Value
     */
    public boolean validateTab() {
        return true;
    }

    /**
     *  Description of the Method
     */
    public void tabSelected() {
    }

    /**
     *  Description of the Method
     */
    public void applyTab() {
        mappingPane.apply();
    }
}
