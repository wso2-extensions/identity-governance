/*
 * Copyright (c) 2019, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.wso2.carbon.identity.governance.model;

import java.io.Serializable;
import java.util.Objects;

/**
 * Object which encapsulates notification template properties.
 */
public class NotificationTemplate implements Serializable {
    
    private static final long serialVersionUID = 1L;

    private String type;
    private String typeKey;
    private String displayName;
    private String locale;
    private String localeKey;
    private String body;
    private String contentType;
    private String notificationChannel;
    private String subject;
    private String footer;

    /**
     * Get notification template subject.
     *
     * @return Notification template subject
     */
    public String getSubject() {

        return subject;
    }

    /**
     * Set notification template subject.
     *
     * @param subject Notification template subject
     */
    public void setSubject(String subject) {

        this.subject = subject;
    }

    /**
     * Get notification template footer.
     *
     * @return Notification template footer
     */
    public String getFooter() {

        return footer;
    }

    /**
     * Set notification template footer.
     *
     * @param footer Notification template footer
     */
    public void setFooter(String footer) {

        this.footer = footer;
    }

    /**
     * Get notification channel content type.
     *
     * @return Notification template content type
     */
    public String getContentType() {

        return contentType;
    }

    /**
     * Set notification channel content type.
     *
     * @param contentType Notification template content type
     */
    public void setContentType(String contentType) {

        this.contentType = contentType;
    }

    /**
     * Get notification channel.
     *
     * @return Notification channel (EMAIL and SMS)
     */
    public String getNotificationChannel() {

        return notificationChannel;
    }

    /**
     * Set notification channel.
     *
     * @param notificationChannel Notification channel (EMAIL and SMS)
     */
    public void setNotificationChannel(String notificationChannel) {

        this.notificationChannel = notificationChannel;
    }

    /**
     * Set notification template type.
     *
     * @return Template type
     */
    public String getType() {

        return type;
    }

    /**
     * Set notification template type.
     *
     * @param type Template type
     */
    public void setType(String type) {

        this.type = type;
        this.typeKey = type != null ? type.toLowerCase() : null;
    }

    /**
     * Get notification template display name.
     *
     * @return Display name
     */
    public String getDisplayName() {

        return displayName;
    }

    /**
     * Set notification template display name.
     *
     * @param displayName Display name
     */
    public void setDisplayName(String displayName) {

        this.displayName = displayName;
    }

    /**
     * Get notification template locale.
     *
     * @return Locale
     */
    public String getLocale() {

        return locale;
    }

    /**
     * Set notification template locale.
     *
     * @param locale Locale
     */
    public void setLocale(String locale) {

        this.locale = locale;
        this.localeKey = locale != null ? locale.toLowerCase() : null;
    }

    /**
     * Get notification template body.
     *
     * @return
     */
    public String getBody() {

        return body;
    }

    /**
     * Set notification body.
     *
     * @param body notification body.
     */
    public void setBody(String body) {

        this.body = body;
    }

    @Override
    public boolean equals(Object o) {

        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        NotificationTemplate that = (NotificationTemplate) o;
        return Objects.equals(typeKey, that.typeKey) && Objects.equals(displayName, that.displayName) &&
                Objects.equals(localeKey, that.localeKey) && Objects.equals(body, that.body) &&
                Objects.equals(contentType, that.contentType) &&
                Objects.equals(notificationChannel, that.notificationChannel) &&
                Objects.equals(subject, that.subject) && Objects.equals(footer, that.footer);
    }

    @Override
    public int hashCode() {

        return Objects.hash(type, displayName, locale, body, contentType, notificationChannel, subject, footer);
    }
}
