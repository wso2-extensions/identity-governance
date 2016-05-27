package org.wso2.carbon.identity.recovery.endpoint.bean;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(propOrder = {
        "status",
        "message"
})
@XmlRootElement(name = "response")
public class StandardResponse {
    private String status;
    private String message;

    public StandardResponse() {
    }

    public StandardResponse(String status, String message) {
        this.status = status;
        this.message = message;
    }

    public StandardResponse(String status) {
        this.status = status;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }
}
